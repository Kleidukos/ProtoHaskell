module Compiler.TypeChecker where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.SrcLoc (unLoc)
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType
import Compiler.Settings (Settings)

import Control.Monad (void)
import Data.Function ((&))
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State

type TypeChecker =
  Eff
    [ Reader UniqueSupply
    , Reader Settings
    , State Environment
    , Error TypeCheckingError
    , IOE
    ]

data Environment = Environment
  { types :: Map Name (PhType Name)
  }
  deriving stock (Eq, Ord, Show)

emptyTypeCheckerEnvironment :: Environment
emptyTypeCheckerEnvironment =
  Environment
    { types = Map.empty
    }

data TypeCheckingError
  = TypeNotFound
  | TypeMismatch
      (PhType Name)
      -- ^ Type to check
      (PhType Name)
      -- ^ Found type
  | OtherTypeError
  deriving stock (Eq, Ord, Show)

runTypeChecker :: Environment -> Settings -> TypeChecker a -> IO (Either TypeCheckingError a)
runTypeChecker env settings action = do
  uniqueSupply <- mkUniqueSupply TypeCheckSection
  action
    & Reader.runReader uniqueSupply
    & Reader.runReader settings
    & State.evalState env
    & Error.runErrorNoCallStack
    & runEff

inferType :: PhDecl Name -> TypeChecker (PhDecl Name)
inferType decl =
  case decl of
    Binding phBind 


inferType' :: PhExpr Name -> TypeChecker TypedExpr
inferType' phExpr = do
  case phExpr of
    PhVar name -> do
      lookedUpType <- lookupTypeOfName name
      pure $ TypedExpr lookedUpType (PhVar name)
    PhLit lit -> do
      synthesisedType <- synthLiteral lit
      pure $ TypedExpr synthesisedType (PhLit lit)
    Typed exprType expression -> do
      checkType (unLoc exprType) (unLoc expression)
    e -> error $ "Did not implement inference for" <> show e

-- | Lookup the type of a term
lookupTypeOfName :: Name -> TypeChecker (PhType Name)
lookupTypeOfName name = do
  Environment{types} <- State.get
  let result =
        Map.elems $
          Map.filterWithKey (\tyName _ -> tyName.occ.nameFS == name.occ.nameFS) types
   in case result of
        [associatedType] -> pure associatedType
        _ -> Error.throwError TypeNotFound

synthLiteral :: PhLit -> TypeChecker (PhType Name)
synthLiteral (LitInt _i) = mkSystemTypeName "Int" >>= mkType
synthLiteral (LitFloat _f) = mkSystemTypeName "Float" >>= mkType
synthLiteral (LitChar _c) = mkSystemTypeName "Char" >>= mkType
synthLiteral (LitString _s) = mkSystemTypeName "String" >>= mkType

mkType :: Name -> TypeChecker (PhType Name)
mkType name = pure $ PhVarTy name

--- Checking

compareType
  :: PhType Name
  -- ^ Type to check
  -> PhType Name
  -- ^ Type in the environment
  -> TypeChecker (PhType Name)
compareType typeToCheck typeFromEnv
  | typeToCheck == typeFromEnv = pure typeToCheck
  | otherwise = Error.throwError $ TypeMismatch typeToCheck typeFromEnv

checkType :: PhType Name -> PhExpr Name -> TypeChecker TypedExpr
checkType typeToCheck expr =
  case expr of
    PhVar name -> do
      (TypedExpr actualType expr') <- inferType (PhVar name)
      void $ compareType typeToCheck actualType
      pure $ TypedExpr actualType expr'
    _ -> error $ "Type checking not implement for " <> show expr

checkMatchGroup :: PhType Name -> MatchGroup Name -> TypeChecker TypedExpr
checkMatchGroup typeToCheck mg = undefined

--
-- checkMatch :: PhType Name -> Match Name -> TypeChecker (Match Name)
-- checkMatch typeToCheck (Match pats body) =
--   case typeToCheck of
--     PhFunTy ty1 ty2 ->
--
checkPat :: PhType Name -> Pat Name -> TypeChecker (Pat Name)
checkPat typeToCheck pat =
  case pat of
    PWildCard -> pure pat
    PLit lit -> do
      typeFromEnv <- synthLiteral lit
      void $ compareType typeToCheck typeFromEnv
      pure pat
    PCon constructorName parameters -> undefined
