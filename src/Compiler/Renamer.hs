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

import Control.Monad
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Lazy.IO qualified as TL
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State
import Text.Pretty.Simple

import Compiler.BasicTypes.FastString
import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhSyn
import Compiler.PhSyn.PhType
import Data.Text.Lazy qualified as TL

data RenamerError
  = NoTopLevelSignature FastString
  | DuplicateBinding FastString -- (Vector SrcSpan)
  | DuplicateSignature FastString
  | BindingNotFound FastString
  deriving stock (Eq, Show)

data RenamerContext = RenamerContext
  { bindings :: Set Name
  , signatures :: Map Name (PhType Name)
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
  RenamerContext{signatures} <- State.get
  parsedModule.modDecls
    & fmap unLoc
    & filter isBinding
    & concatMap
      ( \case
          Binding (FunBind name _) -> [name]
          Binding (PatBind (Located _ (PVar name)) _) -> [name]
          _ -> []
      )
    & mapM_
      ( \name ->
          unless (signatureMember name signatures) $
            Error.throwError $
              NoTopLevelSignature name.occ.nameFS
      )

  printContext "ensureTopLevelSignatures"
  pure parsedModule

-- Renaming ParsedName to Name --

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
  addBinding name
  pure name

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
renamePhDecl (Binding bind) = Binding <$> renamePhBind bind
renamePhDecl (Signature sig) = do
  renamedSignature <- renameSig sig
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
renamePat (PVar name) = do
  PVar <$> renameParsedName name
renamePat rest = traverse renameParsedName rest

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
    renamedCont <- traverse renamePhExpr cont
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
  renamedSigName <- renameSigName sigName
  renamedSigType <- renameSigType sigType
  addSignature renamedSigName (unLoc renamedSigType)
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

addBinding :: Name -> Renamer ()
addBinding name =
  State.modifyM
    ( \env -> do
        if bindingMember name env.bindings
          then Error.throwError $ DuplicateBinding name.occ.nameFS
          else pure $ RenamerContext (Set.insert name env.bindings) env.signatures
    )

addSignature :: Name -> PhType Name -> Renamer ()
addSignature sigName sigType =
  State.modifyM
    ( \env -> do
        if signatureMember sigName env.signatures
          then Error.throwError $ DuplicateSignature sigName.occ.nameFS
          else pure $ RenamerContext env.bindings (Map.insert sigName sigType env.signatures)
    )

guardNotFound :: ParsedName -> Renamer Name
guardNotFound name = do
  RenamerContext{bindings} <- State.get
  renamedName <- renameParsedName name
  if bindingMember renamedName bindings
    then pure renamedName
    else Error.throwError $ BindingNotFound renamedName.occ.nameFS

-- | Determine if a local signature has already been processed.
signatureMember :: Name -> Map Name (PhType Name) -> Bool
signatureMember name sigMap =
  let result =
        length $
          Map.filterWithKey
            (\sig _value -> sig.occ.nameFS == name.occ.nameFS)
            sigMap
   in case result of
        0 -> False
        _ -> True

bindingMember :: Name -> Set Name -> Bool
bindingMember name bindings =
  let result =
        length $
          Set.filter
            (\binding -> binding.occ.nameFS == name.occ.nameFS)
            bindings
   in case result of
        0 -> False
        _ -> True

printContext :: String -> Renamer ()
printContext tag = do
  context <- State.get @RenamerContext
  liftIO $ TL.putStrLn $ pShowNoColorIndent2 [("context_" <> tag, context)]

pShowNoColorIndent2 :: (Show a) => a -> TL.Text
pShowNoColorIndent2 =
  pShowOpt
    defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 2
      , outputOptionsColorOptions = Nothing
      }
