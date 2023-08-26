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
import Data.List qualified as List
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State

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

import Compiler.Settings (Settings)
import Data.Foldable (traverse_)
import Data.Maybe (fromJust, mapMaybe)
import Data.Vector (Vector)
import Effectful.Error.Static
import Prettyprinter
import Utils.MonadUtils
import Utils.Output

runRenamer
  :: Settings
  -> Renamer a
  -> IO (Either (CallStack, RenamerError) a)
runRenamer settings action = do
  uniqueSupply <- mkUniqueSupply RenameSection
  action
    & Reader.runReader uniqueSupply
    & Reader.runReader emptyRenamerContext
    & Reader.runReader settings
    & State.evalState emptyTopLevelContext
    & Error.runError
    & runEff

rename :: Settings -> PhModule ParsedName -> IO (Either (CallStack, RenamerError) (PhModule Name))
rename settings parsedModule = do
  runRenamer settings $ do
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
  traceRenamer $ "Renaming top-level name " <> (outputLazy . pretty $ parsedName)
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
  traceRenamer $ "Renaming ParsedName without registering: " <> (outputLazy . pretty $ parsedName)
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
  traceRenamer $ "Renaming signature name: " <> (outputLazy . pretty $ parsedName)
  unique <- nextUnique
  pure $
    Name
      { sort = Internal
      , occ = parsedNameOcc parsedName
      , uniq = unique
      }

renamePhModule :: PhModule ParsedName -> Renamer (PhModule Name)
renamePhModule parsedModule = do
  traceRenamer $ "Entering " <> maybe "default module" (outputLazy . pretty) parsedModule.modName
  ensureTopLevelSignatures parsedModule.modDecls
  renamedDecls <- traverse (mapLocM renamePhDecl) parsedModule.modDecls
  pure $! Module parsedModule.modName renamedDecls

ensureTopLevelSignatures :: [LPhDecl ParsedName] -> Renamer ()
ensureTopLevelSignatures decls = do
  traceRenamer "Gathering top-level signatures"
  let lol =
        decls
          & fmap unLoc
          & filter (\decl -> isBinding decl || isSignature decl)
  traceRenamer $ outputLazy $ pretty lol
  traverse_
    ( \case
        Binding binding -> Binding <$> renameTopLevelBinding binding
        Signature sig -> do
          Signature <$> renameTopLevelSig sig
        _ -> undefined
    )
    lol
  TopLevelBindings{topLevelBindings, topLevelSignatures} <- State.get
  printContext "Top-level signatures"
  forM_
    topLevelBindings
    ( \name -> do
        traceRenamer $ "Checking top-level binding" <> (outputLazy . pretty $ name)
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
renamePhDecl (Binding bind) = do
  traceRenamer $ "Renaming binding: " <> (outputLazy . pretty $ bind)
  Binding <$> renamePhBindWithoutRegisteringName bind
renamePhDecl (Signature sig) = do
  traceRenamer $ "Renaming signature: " <> (outputLazy . pretty $ sig)
  renamedSignature <- renameTopLevelSigWithoutRegistering sig
  pure $ Signature renamedSignature
renamePhDecl decl =
  error $ "Bleh! I don't know how to rename " <> show decl

renameMatchGroup :: MatchGroup ParsedName -> Renamer (MatchGroup Name)
renameMatchGroup matchGroup = do
  renamedAlternatives <- traverse (mapLocM renameMatch) matchGroup.alternatives
  pure $ MG renamedAlternatives matchGroup.context

renameMatch :: Match ParsedName -> Renamer (Match Name)
renameMatch match = do
  renamedPatterns <- traverse (traverse renamePat) match.matchPats
  renamedRHS <- traverse renameRHS match.rhs
  pure $ Match renamedPatterns renamedRHS

renamePat :: Pat ParsedName -> Renamer (Pat Name)
renamePat (PVar name) = do
  traceRenamer $ "Renaming PVar " <> (outputLazy . pretty $ name)
  PVar <$> renameParsedName name
renamePat rest = traverse renameParsedName rest

renamePatWithoutRegisteringName :: Pat ParsedName -> Renamer (Pat Name)
renamePatWithoutRegisteringName (PVar name) = do
  traceRenamer $ "Renaming PVar without registering: " <> (outputLazy . pretty $ name)
  PVar <$> renameParsedNameWithoutRegistering name
renamePatWithoutRegisteringName rest =  do
  traceRenamer $ "Renaming rest without registering: " <> (outputLazy . pretty $ rest)
  traverse renameParsedName rest

renameTopLevelPat :: Pat ParsedName -> Renamer (Pat Name)
renameTopLevelPat (PVar name) = do 
  traceRenamer $ "Renaming top-level PVar " <> (outputLazy . pretty $ name)
  PVar <$> renameTopLevelName name
renameTopLevelPat rest = do
  traceRenamer $ "Renaming top-level PVar " <> (outputLazy . pretty $ rest)
  traverse renameParsedName rest

renameRHS :: RHS ParsedName -> Renamer (RHS Name)
renameRHS rhs = do
  renamedLocalBinds <- traverse renameBinds rhs.localBinds
  renamedRhs <- traverse renamePhExpr rhs.expr
  pure $ RHS renamedRhs renamedLocalBinds

renamePhExpr :: PhExpr ParsedName -> Renamer (PhExpr Name)
renamePhExpr phExpr =  do
  traceRenamer $ "Renaming rest without registering: " <> (outputLazy . pretty $ phExpr)
  case phExpr of
    PhLit lit -> pure $ PhLit lit
    PhVar name -> PhVar <$> guardNotFound name
    PhLet binds cont -> do
      renamedBinds <- traverse renameBinds binds
      let (names :: Vector Name) =
            mapMaybe (((.name)) . unLoc) (renamedBinds & unLoc & (\(LocalBinds phBinds _) -> phBinds))
              & Vector.fromList
      renamedCont <- addBindings names $ traverse renamePhExpr cont
      pure $ PhLet renamedBinds renamedCont
    parsedExpr -> do
      renamePhExpr parsedExpr

renameBinds :: PhLocalBinds ParsedName -> Renamer (PhLocalBinds Name)
renameBinds (LocalBinds binds signatures) = do
  traceRenamer $ "Renaming top-level PVar " <> (outputLazy . pretty $ binds)
  let groupedBinds = groupBindsByName binds
  uniqueBinds <- guardForDuplicates groupedBinds
  renamedBinds <- traverse (mapLocM renamePhBind) uniqueBinds
  renamedSignatures <- traverse (mapLocM renameSig) signatures
  pure $
    LocalBinds renamedBinds renamedSignatures

guardForDuplicates :: [[LPhBind ParsedName]] -> Renamer [LPhBind ParsedName]
guardForDuplicates groupedBinds = do
  traceRenamer "Guard for duplicates"
  concatMapM
    ( \bindList ->
        if length bindList > 1
          then do
            let duplicateName = bindList & head & unLoc & (.name) & fromJust & (.occ.nameFS)
            let duplicateSpans = bindList & fmap getLoc & Vector.fromList
            throwError (DuplicateBinding duplicateName duplicateSpans)
          else pure bindList
    ) groupedBinds

groupBindsByName :: [LPhBind ParsedName] -> [[LPhBind ParsedName]]
groupBindsByName = List.groupBy (\a b -> (unLoc a).name == (unLoc b).name)

renamePhBind :: PhBind ParsedName -> Renamer (PhBind Name)
renamePhBind (FunBind name matchGroup) = do
  traceRenamer $ "Renaming FunBind " <> (outputLazy . pretty $ name)
  renamedName <- renameParsedName name
  renamedMatchGroup <- renameMatchGroup matchGroup
  pure $
    FunBind renamedName renamedMatchGroup
renamePhBind (PatBind pat body) = do
  traceRenamer $ "Renaming PatBind " <> (outputLazy . pretty $ pat)
  renamedName <- traverse renamePat pat
  renamedBody <- traverse renameRHS body
  pure $ PatBind renamedName renamedBody

renameTopLevelSig :: Sig ParsedName -> Renamer (Sig Name)
renameTopLevelSig (TypeSig sigName sigType) = do
  traceRenamer $ "Renaming top-level Signature: " <> (outputLazy . pretty $ TypeSig sigName sigType)
  renamedSigName <- renameSigName sigName
  renamedSigType <- renameSigType sigType
  addTopLevelSignature renamedSigName (unLoc renamedSigType)
  pure $
    TypeSig renamedSigName renamedSigType

renameTopLevelSigWithoutRegistering :: Sig ParsedName -> Renamer (Sig Name)
renameTopLevelSigWithoutRegistering (TypeSig sigName sigType) = do
  traceRenamer $ "Renaming top-level Signature without registering: " <> (outputLazy . pretty $ TypeSig sigName sigType)
  renamedSigName <- renameSigName sigName
  renamedSigType <- renameSigType sigType
  pure $
    TypeSig renamedSigName renamedSigType

renameSig :: Sig ParsedName -> Renamer (Sig Name)
renameSig (TypeSig sigName sigType) = do
  traceRenamer $ "Renaming signature: " <> (outputLazy . pretty $ TypeSig sigName sigType)
  renamedSigName <- renameSigName sigName
  renamedSigType <- renameSigType sigType
  addTopLevelSignature renamedSigName (unLoc renamedSigType)
  traceRenamer "Added top-level signature"
  pure $
    TypeSig renamedSigName renamedSigType

renameSigType :: LPhType ParsedName -> Renamer (LPhType Name)
renameSigType name = do
  traceRenamer $ "Renaming signature type: " <> (outputLazy . pretty $ name)
  traverse
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
    name

renameTopLevelBinding :: PhBind ParsedName -> Renamer (PhBind Name)
renameTopLevelBinding (FunBind name matchGroup) = do
  traceRenamer $ "Renaming top-level FunBind" <> (outputLazy . pretty $ name) <> " with match group"
  renamedName <- renameTopLevelName name
  renamedMatchGroup <- renameMatchGroup matchGroup
  pure $
    FunBind renamedName renamedMatchGroup
renameTopLevelBinding (PatBind pat body) = do
  traceRenamer $ "Renaming top-level PatBind " <> (outputLazy . pretty $ pat)
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
  renamedName <- renameParsedNameWithoutRegistering name
  if bindingMember renamedName bindings
    then pure renamedName
    else do
      printContext "_guard_not_found"
      Error.throwError $ BindingNotFound renamedName.occ.nameFS
