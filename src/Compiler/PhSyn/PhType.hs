module Compiler.PhSyn.PhType where

import Compiler.BasicTypes.Location
import Data.Text (Text)
import Prettyprinter

data PhType name
  = -- Type variable or type constructor
    PhVarTy NodeID name
  | PhBuiltInTyCon NodeID BuiltInTyCon -- Built in type constructors: (), [], (->), (,), (,,) ...
  | -- | -- Context => type
    --   PhQualTy
    --     [Pred name] -- context (C in C => A)
    --     (PhType name) -- payload (A in C => A)
    PhAppTy NodeID (PhType name) (PhType name)
  | PhFunTy NodeID (PhType name) (PhType name)
  | PhListTy NodeID (PhType name)
  | PhTupleTy NodeID [PhType name]
  | PhParTy NodeID (PhType name) -- parenthesized type
  -- See NOTE: [Par constructors in syn] in Compiler/PhSyn/PhExpr
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data BuiltInTyCon
  = UnitTyCon
  | ListTyCon
  | FunTyCon
  | TupleTyCon Int -- Int is the number of fields; so it should be >= 2
  deriving (Eq, Ord, Show)

data Pred name = IsIn NodeID name (PhType name) -- Eq a, Eq [a] etc.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty name) => Pretty (PhType name) where
  pretty (PhVarTy _ name) = pretty name
  pretty (PhBuiltInTyCon _ tycon) = pretty tycon
  -- pretty (PhQualTy ctxt t) = case ctxt of
  -- [] -> pretty t
  -- [c] -> pretty c <+> pretty @Text "=>" <+> pretty t
  -- cs -> parens (sep (punctuate comma $ map pretty cs)) <+> pretty @Text "=>" <+> pretty t
  pretty (PhAppTy _ t1 t2) = pretty t1 <+> pretty t2
  pretty (PhFunTy _ t1 t2) = pretty t1 <+> pretty @Text "->" <+> pretty t2
  pretty (PhListTy _ t) = brackets $ pretty t
  pretty (PhTupleTy _ ts) = parens $ sep $ punctuate comma $ map pretty ts
  pretty (PhParTy _ t) = parens $ pretty t

instance Pretty BuiltInTyCon where
  pretty UnitTyCon = pretty @Text "()"
  pretty ListTyCon = pretty @Text "[]"
  pretty FunTyCon = pretty @Text "(->)"
  pretty (TupleTyCon f) = parens $ hcat $ replicate (f - 1) comma

instance (Pretty name) => Pretty (Pred name) where
  pretty (IsIn _ name t) = pretty name <+> pretty t
