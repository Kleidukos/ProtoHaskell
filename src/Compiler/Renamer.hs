-- |
-- The Renamer is tasked with:
-- 1. Turning 'ParsedName's into 'Name's.
-- 2. semantic analysis like:
--   * out-of-scope variables
--   * top-level signatures
--   * unused bindings
--   * unused imports
-- 3. Producing the type environment that will be given to the typechecker
module Compiler.Renamer
  ( runRenamer
  , rename
  , RenamerError (..)
  , renameParsedName
  ) where

import Control.Monad
import Data.Function ((&))
import Effectful
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State

-- import Debug.Trace

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhSyn
import Compiler.PhSyn.PhType
import Compiler.Renamer.Types
import Compiler.Renamer.Utils

-- import Data.Text (Text)
import Effectful.Error.Static

runRenamer
  :: Renamer a
  -> IO (Either (CallStack, RenamerError) a)
runRenamer action = do
  uniqueSupply <- mkUniqueSupply RenameSection
  action
    & Reader.runReader uniqueSupply
    & Reader.runReader emptyRenamerContext
    & State.evalState emptyTopLevelContext
    & Error.runError
    & runEff

rename :: PhModule ParsedName -> IO (Either (CallStack, RenamerError) (PhModule Name))
rename parsedModule =
  runRenamer $
    renamePhModule parsedModule

-- Renaming ParsedName to Name --

-----------------------
--- Individual renamers
-----------------------

-- | Do not use this function to rename signature names!
renameParsedName :: ParsedName -> Renamer Name
renameParsedName parsedName = do
  unique <- nextUnique
  let name =
        Name
          { sort = Internal
          , occ = parsedNameOcc parsedName
          , uniq = unique
          }
  addBinding name $
    pure name

renameTopLevelName :: ParsedName -> Renamer Name
renameTopLevelName parsedName = do
  unique <- nextUnique
  let name =
        Name
          { sort = Internal
          , occ = parsedNameOcc parsedName
          , uniq = unique
          }
  addTopLevelBinding name
  pure name

renameParsedNameWithoutRegistering :: ParsedName -> Renamer Name
renameParsedNameWithoutRegistering parsedName = do
  unique <- nextUnique
  pure $
    Name
      { sort = Internal
      , occ = parsedNameOcc parsedName
      , uniq = unique
      }

-- | Like 'renameParsedName' but do not add it to the bindings environment
renameSigName :: ParsedName -> Renamer Name
renameSigName parsedName = do
  unique <- nextUnique
  pure $
    Name
      { sort = Internal
      , occ = parsedNameOcc parsedName
      , uniq = unique
      }

renamePhModule :: PhModule ParsedName -> Renamer (PhModule Name)
renamePhModule parsedModule = do
  ensureTopLevelSignatures (parsedModule.modDecls)
  renamedDecls <- traverse (mapLocM renamePhDecl) parsedModule.modDecls
  pure $! Module parsedModule.modName renamedDecls

ensureTopLevelSignatures :: [LPhDecl ParsedName] -> Renamer ()
ensureTopLevelSignatures decls = do
  decls
    & fmap unLoc
    & filter (\decl -> isBinding decl || isSignature decl)
    & traverse
      ( \case
          Binding binding -> Binding <$> renameTopLevelBinding binding
          Signature sig -> Signature <$> renameTopLevelSig sig
          _ -> undefined
      )
  TopLevelBindings{topLevelBindings, topLevelSignatures} <- State.get
  forM_
    topLevelBindings
    ( \name ->
        unless (signatureMember name topLevelSignatures) $
          Error.throwError $
            NoTopLevelSignature name.occ.nameFS
    )

------------------------
--- Declaration renamers
------------------------

-- | renamePhDecl is the entry point into the top-level
-- data, type and term declarations.
-- It presupposes that 'ensureTopLevelSignatures' has
-- alredy been run, and does not perform the registration
-- of top-level type and term declaration,
-- otherwise we would be getting duplicates.
renamePhDecl :: PhDecl ParsedName -> Renamer (PhDecl Name)
renamePhDecl (Binding bind) = Binding <$> renamePhBindWithoutRegisteringName bind
renamePhDecl (Signature sig) = do
  renamedSignature <- renameTopLevelSigWithoutRegistering sig
  pure $ Signature renamedSignature
renamePhDecl decl =
  error $ "Bleh! I don't know how to rename " <> show decl

renameMatchGroup :: MatchGroup ParsedName -> Renamer (MatchGroup Name)
renameMatchGroup matchGroup = do
  renamedAlternatives <- traverse (mapLocM renameMatch) (matchGroup.alternatives)
  pure $ MG renamedAlternatives matchGroup.context

renameMatch :: Match ParsedName -> Renamer (Match Name)
renameMatch match = do
  renamedPatterns <- traverse (traverse renamePat) match.matchPats
  renamedRHS <- traverse renameRHS match.rhs
  pure $ Match renamedPatterns renamedRHS

renamePat :: Pat ParsedName -> Renamer (Pat Name)
renamePat (PVar name) =
  PVar <$> renameParsedName name
renamePat rest = traverse renameParsedName rest

renamePatWithoutRegisteringName :: Pat ParsedName -> Renamer (Pat Name)
renamePatWithoutRegisteringName (PVar name) =
  PVar <$> renameParsedNameWithoutRegistering name
renamePatWithoutRegisteringName rest = traverse renameParsedName rest

renameTopLevelPat :: Pat ParsedName -> Renamer (Pat Name)
renameTopLevelPat (PVar name) = do
  PVar <$> renameTopLevelName name
renameTopLevelPat rest = traverse renameParsedName rest

renameRHS :: RHS ParsedName -> Renamer (RHS Name)
renameRHS rhs = do
  renamedLocalBinds <- traverse renameBinds rhs.localBinds
  renamedRhs <- traverse renamePhExpr rhs.expr
  pure $ RHS renamedRhs renamedLocalBinds

renamePhExpr :: PhExpr ParsedName -> Renamer (PhExpr Name)
renamePhExpr = \case
  PhLit lit -> pure $ PhLit lit
  PhVar name -> PhVar <$> guardNotFound name
  PhLet binds cont -> do
    renamedBinds <- traverse renameBinds binds
    printContext "_renamed binds"
    -- liftIO $ print ("renamed binds" :: Text, renamedBinds)
    renamedCont <- traverse renamePhExpr cont
    pure $ PhLet renamedBinds renamedCont
  parsedExpr -> do
    renamePhExpr parsedExpr

renameBinds :: PhLocalBinds ParsedName -> Renamer (PhLocalBinds Name)
renameBinds localBinds = do
  let LocalBinds binds signatures = localBinds
  renamedBinds <- traverse (mapLocM renamePhBind) binds
  printContext "_after_renaming_binds"
  renamedSignatures <- traverse (mapLocM renameSig) signatures
  pure $
    LocalBinds renamedBinds renamedSignatures

renamePhBind :: PhBind ParsedName -> Renamer (PhBind Name)
renamePhBind (FunBind name matchGroup) = do
  renamedName <- renameParsedName name
  renamedMatchGroup <- renameMatchGroup matchGroup
  pure $
    FunBind renamedName renamedMatchGroup
renamePhBind (PatBind pat body) = do
  renamedName <- traverse renamePat pat
  renamedBody <- traverse renameRHS body
  pure $ PatBind renamedName renamedBody

renameTopLevelSig :: Sig ParsedName -> Renamer (Sig Name)
renameTopLevelSig (TypeSig sigName sigType) = do
  renamedSigName <- renameSigName sigName
  renamedSigType <- renameSigType sigType
  addTopLevelSignature renamedSigName (unLoc renamedSigType)
  pure $
    TypeSig renamedSigName renamedSigType

renameTopLevelSigWithoutRegistering :: Sig ParsedName -> Renamer (Sig Name)
renameTopLevelSigWithoutRegistering (TypeSig sigName sigType) = do
  renamedSigName <- renameSigName sigName
  renamedSigType <- renameSigType sigType
  pure $
    TypeSig renamedSigName renamedSigType

renameSig :: Sig ParsedName -> Renamer (Sig Name)
renameSig (TypeSig sigName sigType) = do
  renamedSigName <- renameSigName sigName
  renamedSigType <- renameSigType sigType
  addTopLevelSignature renamedSigName (unLoc renamedSigType)
  pure $
    TypeSig renamedSigName renamedSigType

renameSigType :: LPhType ParsedName -> Renamer (LPhType Name)
renameSigType =
  mapLocM
    ( \case
        PhVarTy parsedName -> PhVarTy <$> renameSigName parsedName
        PhBuiltInTyCon tyCon -> pure $ PhBuiltInTyCon tyCon
        PhAppTy parsedName1 parsedName2 ->
          PhAppTy <$> renameSigType parsedName1 <*> renameSigType parsedName2
        PhFunTy parsedName1 parsedName2 ->
          PhFunTy <$> renameSigType parsedName1 <*> renameSigType parsedName2
        PhListTy parsedName -> PhListTy <$> renameSigType parsedName
        PhTupleTy parsedNames -> PhTupleTy <$> traverse renameSigType parsedNames
        PhParTy parsedName -> PhParTy <$> renameSigType parsedName
    )

renameTopLevelBinding :: PhBind ParsedName -> Renamer (PhBind Name)
renameTopLevelBinding (FunBind name matchGroup) = do
  renamedName <- renameTopLevelName name
  renamedMatchGroup <- renameMatchGroup matchGroup
  pure $
    FunBind renamedName renamedMatchGroup
renameTopLevelBinding (PatBind pat body) = do
  renamedName <- traverse renameTopLevelPat pat
  renamedBody <- traverse renameRHS body
  pure $ PatBind renamedName renamedBody

renamePhBindWithoutRegisteringName :: PhBind ParsedName -> Renamer (PhBind Name)
renamePhBindWithoutRegisteringName (FunBind name matchGroup) = do
  renamedName <- renameParsedNameWithoutRegistering name
  renamedMatchGroup <- renameMatchGroup matchGroup
  pure $
    FunBind renamedName renamedMatchGroup
renamePhBindWithoutRegisteringName (PatBind pat body) = do
  renamedName <- traverse renamePatWithoutRegisteringName pat
  renamedBody <- traverse renameRHS body
  pure $ PatBind renamedName renamedBody

guardNotFound :: ParsedName -> Renamer Name
guardNotFound name = do
  RenamerContext{bindings} <- Reader.ask
  renamedName <- renameParsedName name
  if bindingMember renamedName bindings
    then pure renamedName
    else Error.throwError $ BindingNotFound renamedName.occ.nameFS
