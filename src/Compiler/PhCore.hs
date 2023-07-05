module Compiler.PhCore where

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhExpr

data CoreExpr
  = CoreVar Name
  | CoreLit PhLit
  | CoreApp CoreExpr CoreExpr
  | CoreLam Name CoreExpr
  | NegApp CoreExpr
  | -- Parenthesized expr, see NOTE: [Par constructors in syn]
    CorePar CoreExpr
  | CoreCase CoreExpr [AltBranch]
  | CoreLet Bind CoreExpr
  | CoreDo [CoreStatement]
  | ExplicitTuple [CoreExpr]
  | ExplicitList [CoreExpr]
  deriving (Eq, Ord, Show)

data Bind = Bind
  { bindName :: Name
  , expr :: CoreExpr
  }
  deriving stock (Eq, Ord, Show)

-- | The stuff we get in a
data CoreStatement
  = StatementExpr CoreExpr
  | StatementLet Name CoreExpr
  deriving (Eq, Ord, Show)

data AltCon
  = AltDataCon DataCon
  | AltLiteral PhLit
  | AltDefault
  deriving (Eq, Ord, Show)

data DataCon = DataCon
  { name :: Name
  , unique :: Unique
  }
  deriving (Eq, Ord, Show)

data AltBranch = AltBranch
  { altCon :: AltCon
  -- ^ Constructor leading the alternative
  , bindings :: [Name]
  -- ^ Variables bound in the match
  , body :: CoreExpr
  -- ^ Branch to be executed
  }
  deriving (Eq, Ord, Show)

-- ANF

data ImmediateExpr
  = ImmNum Int
  | ImmVar Name
  -- Complete
  deriving (Eq, Ord, Show)

data LetExpr
  = Halt CoreExpr
  | ALet
      Name
      -- ^ Var
      CoreExpr
      -- ^ The operation
      LetExpr
      -- ^ The rest
  deriving (Eq, Ord, Show)
