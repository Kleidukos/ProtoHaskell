{-# LANGUAGE LambdaCase #-}
module Compiler.TypeChecker where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local (State, state, gets)

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType
import Control.Monad (void)

data TypeCheckingError
  = UnboundVariable Name
  | ExpectedFunctionType (LPhExpr Name) (PhType Name)
  deriving stock (Eq, Show)

data TypeCheckerContext = TypeCheckerContext
  { types :: Map Name (PhType Name)
  , uniqueSupply :: UniqueSupply
  }
  deriving stock (Eq, Show)

type TypeChecker a = Eff [State TypeCheckerContext, Error TypeCheckingError, IOE] a

nextUnique :: TypeChecker Unique
nextUnique = do
  state
    ( \context ->
        let s = context.uniqueSupply.uniques
            (returnValue, newSupply) = (head s, UniqueSupply $ tail s)
            newState = context{uniqueSupply = newSupply}
         in (returnValue, newState)
    )

check :: PhType Name -> PhExpr Name -> TypeChecker ()
check expectedName = \case
  PhLam matchgroup -> 
    

infer :: PhExpr Name -> TypeChecker (PhType Name)
infer = \case
  PhLit lit -> inferLit lit
  PhVar name -> inferVar name
  PhApp locatedExpr1 locatedExpr2 -> inferApplication locatedExpr1 locatedExpr2

inferLit :: PhLit -> TypeChecker (PhType Name)
inferLit (LitInt _) = do 
  unique <- nextUnique
  pure $ PhVarTy $ Name Internal (mkClsOccName (UnhelpfulSpan "lol") "Int") unique
inferLit (LitFloat _) = do
  unique <- nextUnique
  pure $ PhVarTy $ Name Internal (mkClsOccName (UnhelpfulSpan "lol") "Float") unique
inferLit (LitChar _) = do
  unique <- nextUnique
  pure $ PhVarTy $ Name Internal (mkClsOccName (UnhelpfulSpan "lol") "Char") unique
inferLit (LitString _) = do
  unique <- nextUnique
  pure $ PhVarTy $ Name Internal (mkClsOccName (UnhelpfulSpan "lol") "Text") unique

inferVar :: Name -> TypeChecker (PhType Name)
inferVar name = gets @TypeCheckerContext (\context -> Map.lookup name context.types) >>= \case
  Just ty -> pure ty
  Nothing -> throwError $ UnboundVariable name

inferApplication :: LPhExpr Name -> LPhExpr Name -> TypeChecker (PhType Name)
inferApplication e1 e2 = do
  Located e1Loc e1Type <- mapLocM infer e1
  case e1Type of
    PhFunTy (Located t1Loc t1) (Located t2Loc t2) -> do
      mapLocM (check t1) e2
      pure $ PhFunTy (Located e1Loc e1Type) (Located t2Loc t2)
    t -> throwError $ ExpectedFunctionType e1 t
