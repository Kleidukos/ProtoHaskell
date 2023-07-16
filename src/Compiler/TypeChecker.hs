module Compiler.TypeChecker where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType
import Effectful
import Effectful.Error.Static (Error)

import Compiler.BasicTypes.Location
import Data.Function ((&))
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State

type TypeChecker =
  Eff
    [ Reader UniqueSupply
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

runTypeChecker :: Environment -> TypeChecker a -> IO (Either TypeCheckingError a)
runTypeChecker env action = do
  uniqueSupply <- mkUniqueSupply TypeCheckSection
  action
    & Reader.runReader uniqueSupply
    & State.evalState env
    & Error.runErrorNoCallStack
    & runEff

inferType :: PhExpr Name -> TypeChecker (PhType Name)
inferType = \case
  PhVar _ name -> lookupType name
  PhLit nodeID lit -> synthLiteral nodeID lit
  Typed _ exprType expression -> checkType expression exprType

-- | Lookup the type of a term
lookupType :: Name -> TypeChecker (PhType Name)
lookupType name = do
  Environment{types} <- State.get
  let result = Map.elems $ Map.filterWithKey (\tyName _ -> tyName.occ.nameFS == name.occ.nameFS) types
   in case result of
        [associatedType] -> pure associatedType
        _ -> Error.throwError TypeNotFound

synthLiteral :: NodeID -> PhLit -> TypeChecker (PhType Name)
synthLiteral nodeID (LitInt _i) = mkTypeName "Int" >>= mkType nodeID
synthLiteral nodeID (LitFloat _f) = mkTypeName "Float" >>= mkType nodeID
synthLiteral nodeID (LitChar _c) = mkTypeName "Char" >>= mkType nodeID
synthLiteral nodeID (LitString _s) = mkTypeName "String" >>= mkType nodeID

mkType :: NodeID -> Name -> TypeChecker (PhType Name)
mkType nodeID name = pure $ PhVarTy nodeID name

--- Checking

checkType :: PhExpr Name -> PhType Name -> TypeChecker (PhType Name)
checkType term typeToCheck = do
  foundType <- inferType term
  if typeToCheck == foundType
    then pure typeToCheck
    else Error.throwError $ TypeMismatch typeToCheck foundType
