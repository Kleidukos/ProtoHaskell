{-# OPTIONS_GHC -Wno-unused-imports #-}

module Compiler.PhCore where

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhExpr

data CoreExpr
  = CoreVar Name
  | CoreLit PhLit
  | CoreLam Name CoreExpr
  | CoreApp CoreExpr CoreExpr
  | OpApp
      CoreExpr -- left operand
      CoreExpr -- operator, ALWAYS a CoreVar
      CoreExpr -- right operand
  | NegApp CoreExpr
  | -- Parenthesized expr, see NOTE: [Par constructors in syn]
    CorePar CoreExpr
  | CoreCase CoreExpr [AltBranch]
  | CoreIf
      CoreExpr -- predicate
      CoreExpr -- consequent
      CoreExpr -- alternative
  | CoreLet Name CoreExpr
  | CoreDo [CoreStatement]
  | ExplicitTuple [CoreExpr]
  | ExplicitList [CoreExpr]
  -- | ArithSeq ArithSeqInfo
  -- | Typed LPhType CoreExpr
  deriving (Eq, Ord, Show)

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
