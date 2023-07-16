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

-- import Debug.Trace

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhSyn
import Compiler.PhSyn.PhType
import Compiler.Renamer.Types
import Compiler.Renamer.Utils

-- import Data.Text (Text)

import Data.Maybe (fromJust, mapMaybe)
import Data.Vector (Vector)
import Effectful.Error.Static
import Utils.MonadUtils

runRenamer
  :: Renamer a
  -> IO (Either RenamerError a)
runRenamer action = do
  uniqueSupply <- mkUniqueSupply RenameSection
  action
    & Reader.runReader uniqueSupply
    & Reader.runReader emptyRenamerContext
    & State.evalState emptyTopLevelContext
    & Error.runErrorNoCallStack
    & runEff

rename :: PhModule ParsedName -> IO (Either RenamerError (PhModule Name))
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
  renamedDecls <- traverse renamePhDecl parsedModule.modDecls
  pure $! Module parsedModule.nodeID parsedModule.modName renamedDecls

ensureTopLevelSignatures :: [PhDecl ParsedName] -> Renamer ()
ensureTopLevelSignatures decls = do
  decls
    & filter (\decl -> isBinding decl || isSignature decl)
    & traverse
      ( \case
          Binding nodeID binding -> Binding nodeID <$> renameTopLevelBinding binding
          Signature nodeID sig -> Signature nodeID <$> renameTopLevelSig sig
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
renamePhDecl (Binding nodeID bind) = Binding nodeID <$> renamePhBindWithoutRegisteringName bind
renamePhDecl (Signature nodeID sig) = do
  renamedSignature <- renameTopLevelSigWithoutRegistering sig
  pure $ Signature nodeID renamedSignature
renamePhDecl decl =
  error $ "Bleh! I don't know how to rename " <> show decl

renameMatchGroup :: MatchGroup ParsedName -> Renamer (MatchGroup Name)
renameMatchGroup matchGroup = do
  renamedAlternatives <- traverse renameMatch (matchGroup.alternatives)
  pure $ MG matchGroup.nodeID renamedAlternatives matchGroup.context

renameMatch :: Match ParsedName -> Renamer (Match Name)
renameMatch match = do
  renamedPatterns <- traverse renamePat match.matchPats
  renamedRHS <- renameRHS match.rhs
  pure $ Match match.nodeID renamedPatterns renamedRHS

renamePat :: Pat ParsedName -> Renamer (Pat Name)
renamePat (PVar nodeID name) =
  PVar nodeID <$> renameParsedName name
renamePat rest = traverse renameParsedName rest

renamePatWithoutRegisteringName :: Pat ParsedName -> Renamer (Pat Name)
renamePatWithoutRegisteringName (PVar nodeID name) =
  PVar nodeID <$> renameParsedNameWithoutRegistering name
renamePatWithoutRegisteringName rest = traverse renameParsedName rest

renameTopLevelPat :: Pat ParsedName -> Renamer (Pat Name)
renameTopLevelPat (PVar nodeID name) = PVar nodeID <$> renameTopLevelName name
renameTopLevelPat rest = traverse renameParsedName rest

renameRHS :: RHS ParsedName -> Renamer (RHS Name)
renameRHS rhs = do
  renamedLocalBinds <- renameBinds rhs.localBinds
  renamedRhs <- renamePhExpr rhs.expr
  pure $ RHS rhs.nodeID renamedRhs renamedLocalBinds

renamePhExpr :: PhExpr ParsedName -> Renamer (PhExpr Name)
renamePhExpr = \case
  PhLit nodeID lit -> pure $ PhLit nodeID lit
  PhVar nodeID name -> PhVar nodeID <$> guardNotFound name
  PhLet nodeID binds cont -> do
    renamedBinds <- renameBinds binds
    let (names :: Vector Name) =
          mapMaybe
            (.name)
            ( renamedBinds
                & (\(LocalBinds _ phBinds _) -> phBinds)
            )
            & Vector.fromList
    renamedCont <- addBindings names $ renamePhExpr cont
    pure $ PhLet nodeID renamedBinds renamedCont
  parsedExpr -> do
    renamePhExpr parsedExpr

renameBinds :: PhLocalBinds ParsedName -> Renamer (PhLocalBinds Name)
renameBinds (LocalBinds nodeID binds signatures) = do
  let groupedBinds = groupBindsByName binds
  uniqueBinds <- guardForDuplicates groupedBinds
  renamedBinds <- traverse renamePhBind uniqueBinds
  renamedSignatures <- traverse renameSig signatures
  pure $
    LocalBinds nodeID renamedBinds renamedSignatures

guardForDuplicates :: [[PhBind ParsedName]] -> Renamer [PhBind ParsedName]
guardForDuplicates =
  concatMapM
    ( \bindList ->
        if length bindList > 1
          then do
            let duplicateName = bindList & head & (.name) & fromJust & (.occ.nameFS)
            let duplicateSpans = bindList & fmap (.nodeID) & Vector.fromList
            throwError (DuplicateBinding duplicateName duplicateSpans)
          else pure bindList
    )

groupBindsByName :: [PhBind ParsedName] -> [[PhBind ParsedName]]
groupBindsByName = List.groupBy (\a b -> a.name == b.name)

renamePhBind :: PhBind ParsedName -> Renamer (PhBind Name)
renamePhBind (FunBind nodeID name matchGroup) = do
  renamedName <- renameParsedName name
  renamedMatchGroup <- renameMatchGroup matchGroup
  pure $
    FunBind nodeID renamedName renamedMatchGroup
renamePhBind (PatBind nodeID pat body) = do
  renamedName <- renamePat pat
  renamedBody <- renameRHS body
  pure $ PatBind nodeID renamedName renamedBody

renameTopLevelSig :: Sig ParsedName -> Renamer (Sig Name)
renameTopLevelSig (TypeSig nodeID sigName sigType) = do
  renamedSigName <- renameSigName sigName
  renamedSigType <- renameSigType sigType
  addTopLevelSignature renamedSigName renamedSigType
  pure $
    TypeSig nodeID renamedSigName renamedSigType

renameTopLevelSigWithoutRegistering :: Sig ParsedName -> Renamer (Sig Name)
renameTopLevelSigWithoutRegistering (TypeSig nodeID sigName sigType) = do
  renamedSigName <- renameSigName sigName
  renamedSigType <- renameSigType sigType
  pure $
    TypeSig nodeID renamedSigName renamedSigType

renameSig :: Sig ParsedName -> Renamer (Sig Name)
renameSig (TypeSig nodeID sigName sigType) = do
  renamedSigName <- renameSigName sigName
  renamedSigType <- renameSigType sigType
  addTopLevelSignature renamedSigName renamedSigType
  pure $
    TypeSig nodeID renamedSigName renamedSigType

renameSigType :: PhType ParsedName -> Renamer (PhType Name)
renameSigType = \case
  PhVarTy nodeID parsedName -> PhVarTy nodeID <$> renameSigName parsedName
  PhBuiltInTyCon nodeID tyCon -> pure $ PhBuiltInTyCon nodeID tyCon
  PhAppTy nodeID parsedName1 parsedName2 ->
    PhAppTy nodeID <$> renameSigType parsedName1 <*> renameSigType parsedName2
  PhFunTy nodeID parsedName1 parsedName2 ->
    PhFunTy nodeID <$> renameSigType parsedName1 <*> renameSigType parsedName2
  PhListTy nodeID parsedName -> PhListTy nodeID <$> renameSigType parsedName
  PhTupleTy nodeID parsedNames -> PhTupleTy nodeID <$> traverse renameSigType parsedNames
  PhParTy nodeID parsedName -> PhParTy nodeID <$> renameSigType parsedName

renameTopLevelBinding :: PhBind ParsedName -> Renamer (PhBind Name)
renameTopLevelBinding (FunBind nodeID name matchGroup) = do
  renamedName <- renameTopLevelName name
  renamedMatchGroup <- renameMatchGroup matchGroup
  pure $
    FunBind nodeID renamedName renamedMatchGroup
renameTopLevelBinding (PatBind nodeID pat body) = do
  renamedName <- renameTopLevelPat pat
  renamedBody <- renameRHS body
  pure $ PatBind nodeID renamedName renamedBody

renamePhBindWithoutRegisteringName :: PhBind ParsedName -> Renamer (PhBind Name)
renamePhBindWithoutRegisteringName (FunBind nodeID name matchGroup) = do
  renamedName <- renameParsedNameWithoutRegistering name
  renamedMatchGroup <- renameMatchGroup matchGroup
  pure $
    FunBind nodeID renamedName renamedMatchGroup
renamePhBindWithoutRegisteringName (PatBind nodeID pat body) = do
  renamedName <- renamePatWithoutRegisteringName pat
  renamedBody <- renameRHS body
  pure $ PatBind nodeID renamedName renamedBody

guardNotFound :: ParsedName -> Renamer Name
guardNotFound name = do
  RenamerContext{bindings} <- Reader.ask
  renamedName <- renameParsedNameWithoutRegistering name
  if bindingMember renamedName bindings
    then pure renamedName
    else do
      printContext "_guard_not_found"
      Error.throwError $ BindingNotFound renamedName.occ.nameFS
