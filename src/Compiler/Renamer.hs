{-# LANGUAGE DataKinds #-}

-- |
-- The Renamer is tasked with:
-- 1. Turning 'ParsedName's into 'Name's.
-- 2. lexical analysis like:
--   * out-of-scope variables
--   * unused bindings
--   * unused imports
-- 3. Producing the type environment that will be given to the typechecker
module Compiler.Renamer
  ( runRenamer
  , rename
  , RenamerError (..)
  , renameParsedName
  ) where

import Compiler.BasicTypes.FastString
import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhSyn
import Compiler.PhSyn.PhType

import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State

data RenamerError
  = NoTopLevelSig
  | DuplicateBinding FastString -- (Vector SrcSpan)
  | BindingNotFound FastString
  deriving stock (Eq, Show)

data RenamerContext = RenamerContext
  { bindings :: Set ParsedName
  , signatures :: Map ParsedName (PhType ParsedName)
  }
  deriving stock (Eq, Show)

type Renamer a =
  Eff
    [ Reader UniqueSupply
    , State RenamerContext
    , Error RenamerError
    , IOE
    ]
    a

runRenamer
  :: Renamer a
  -> IO (Either RenamerError a)
runRenamer action = do
  uniqueSupply <- mkUniqueSupply RenameSection
  action
    & Reader.runReader uniqueSupply
    & State.evalState emptyRenamerContext
    & Error.runErrorNoCallStack
    & runEff

rename :: PhModule ParsedName -> IO (Either RenamerError (PhModule Name))
rename parsedModule =
  runRenamer $
    renamePhModule parsedModule
      >>= ensureTopLevelSignatures

emptyRenamerContext :: RenamerContext
emptyRenamerContext =
  RenamerContext
    { bindings = Set.empty
    , signatures = Map.empty
    }

-- Top-level binding analysis: must have type signature --

-- We need to find the signatures with the same nameFS as the bindings.
-- Simply looking up 'Name's cannot work.
ensureTopLevelSignatures :: PhModule Name -> Renamer (PhModule Name)
ensureTopLevelSignatures parsedModule = do
  let decls = fmap unLoc parsedModule.modDecls
  pure parsedModule

extractTopLevelBindings :: [PhDecl Name] -> [Name]
extractTopLevelBindings decls = bindings
  where
    bindingDecls = foldMap getBinding decls
    bindings =
      foldMap
        ( \case
            FunBind name _ -> [name]
            _ -> []
        )
        bindingDecls

getBinding :: PhDecl Name -> [PhBind Name]
getBinding (Binding b) = [b]
getBinding _ = []

-- Renaming ParsedName to Name --

renameParsedName :: ParsedName -> Renamer Name
renameParsedName parsedName = do
  unique <- nextUnique
  addBinding parsedName
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
  renamedDecls <- traverse (mapLocM renamePhDecl) parsedModule.modDecls
  pure $! Module parsedModule.modName renamedDecls

renamePhDecl :: PhDecl ParsedName -> Renamer (PhDecl Name)
renamePhDecl (Binding (FunBind name matchGroup)) = do
  renamedName <- guardDuplicate name
  renamedMatchGroup <- renameMatchGroup matchGroup
  pure $ Binding $ FunBind renamedName renamedMatchGroup
renamePhDecl decl = traverse renameParsedName decl

renameMatchGroup :: MatchGroup ParsedName -> Renamer (MatchGroup Name)
renameMatchGroup matchGroup = do
  renamedAlternatives <- traverse (mapLocM renameMatch) (matchGroup.alternatives)
  pure $ MG renamedAlternatives matchGroup.context

renameMatch :: Match ParsedName -> Renamer (Match Name)
renameMatch match = do
  renamedPatterns <- traverse (mapLocM renameParsedName) match.matchPats
  renamedRHS <- mapLocM renameRHS match.rhs
  pure $ Match renamedPatterns renamedRHS

renameRHS :: RHS ParsedName -> Renamer (RHS Name)
renameRHS rhs = do
  renamedRhs <- mapLocM renamePhExpr rhs.expr
  renamedLocalBinds <- mapLocM renameBinds rhs.localBinds
  pure $ RHS renamedRhs renamedLocalBinds

renamePhExpr :: PhExpr ParsedName -> Renamer (PhExpr Name)
renamePhExpr expr = case expr of
  PhLit lit -> pure $ PhLit lit
  PhVar name -> PhVar <$> guardNotFound name
  PhLet binds cont -> do
    renamedBinds <- mapLocM renameBinds binds
    renamedCont <- mapLocM renamePhExpr cont
    pure $ PhLet renamedBinds renamedCont
  parsedExpr -> do
    renamePhExpr parsedExpr

renameBinds :: PhLocalBinds ParsedName -> Renamer (PhLocalBinds Name)
renameBinds localBinds = do
  let LocalBinds binds signatures = localBinds
  renamedBinds <- traverse (mapLocM renamePhBind) binds
  renamedSignatures <- traverse (mapLocM renameSig) signatures
  pure $
    LocalBinds renamedBinds renamedSignatures

renameSig :: Sig ParsedName -> Renamer (Sig Name)
renameSig (TypeSig sigName sigType) = do
  addSignature sigName (unLoc sigType)
  renamedSigName :: Name <- renameSigName sigName
  renamedSigType <- renameSigType sigType
  pure $
    TypeSig renamedSigName renamedSigType

renameSigType :: LPhType ParsedName -> Renamer (LPhType Name)
renameSigType = undefined

renamePhBind :: PhBind ParsedName -> Renamer (PhBind Name)
renamePhBind (FunBind name matchGroup) = do
  renamedName <- guardDuplicate name
  renamedMatchGroup <- renameMatchGroup matchGroup
  pure $
    FunBind renamedName renamedMatchGroup

guardDuplicate :: ParsedName -> Renamer Name
guardDuplicate nameToCheck = do
  RenamerContext{bindings} <- State.get
  if Set.member nameToCheck bindings
    then Error.throwError $ DuplicateBinding nameToCheck.occ.nameFS
    else renameParsedName nameToCheck

addBinding :: ParsedName -> Renamer ()
addBinding name =
  State.modify
    (\env -> RenamerContext (Set.insert name env.bindings) env.signatures)

addSignature :: ParsedName -> PhType ParsedName -> Renamer ()
addSignature sigName sigType =
  State.modify
    (\env -> RenamerContext env.bindings (Map.insert sigName sigType env.signatures))

guardNotFound :: ParsedName -> Renamer Name
guardNotFound name = do
  RenamerContext{bindings} <- State.get
  if Set.member name bindings
    then renameParsedName name
    else Error.throwError $ BindingNotFound name.occ.nameFS
