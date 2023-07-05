module Compiler.PhSyn.PhType where

import Compiler.BasicTypes.SrcLoc
import Data.Text (Text)
import Prettyprinter

type LPhType name = Located (PhType name)
data PhType name
  = -- Type variable or type constructor
    PhVarTy name
  | PhBuiltInTyCon BuiltInTyCon -- Built in type constructors: (), [], (->), (,), (,,) ...
  | -- | -- Context => type
    --   PhQualTy
    --     [Pred name] -- context (C in C => A)
    --     (LPhType name) -- payload (A in C => A)
    PhAppTy (LPhType name) (LPhType name)
  | PhFunTy (LPhType name) (LPhType name)
  | PhListTy (LPhType name)
  | PhTupleTy [LPhType name]
  | PhParTy (LPhType name) -- parenthesized type
  -- See NOTE: [Par constructors in syn] in Compiler/PhSyn/PhExpr
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data BuiltInTyCon
  = UnitTyCon
  | ListTyCon
  | FunTyCon
  | TupleTyCon Int -- Int is the number of fields; so it should be >= 2
  deriving (Eq, Ord, Show)

data Pred name = IsIn name (PhType name) -- Eq a, Eq [a] etc.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty name) => Pretty (PhType name) where
  pretty (PhVarTy name) = pretty name
  pretty (PhBuiltInTyCon tycon) = pretty tycon
  -- pretty (PhQualTy ctxt t) = case ctxt of
  -- [] -> pretty t
  -- [c] -> pretty c <+> pretty @Text "=>" <+> pretty t
  -- cs -> parens (sep (punctuate comma $ map pretty cs)) <+> pretty @Text "=>" <+> pretty t
  pretty (PhAppTy t1 t2) = pretty t1 <+> pretty t2
  pretty (PhFunTy t1 t2) = pretty t1 <+> pretty @Text "->" <+> pretty t2
  pretty (PhListTy t) = brackets $ pretty t
  pretty (PhTupleTy ts) = parens $ sep $ punctuate comma $ map pretty ts
  pretty (PhParTy t) = parens $ pretty t

instance Pretty BuiltInTyCon where
  pretty UnitTyCon = pretty @Text "()"
  pretty ListTyCon = pretty @Text "[]"
  pretty FunTyCon = pretty @Text "(->)"
  pretty (TupleTyCon f) = parens $ hcat $ replicate (f - 1) comma

instance (Pretty name) => Pretty (Pred name) where
  pretty (IsIn name t) = pretty name <+> pretty t

mkLPhFunTy :: LPhType name -> LPhType name -> LPhType name
mkLPhFunTy t1@(Located span1 _) t2@(Located span2 _) =
  Located (combineSrcSpans span1 span2) (PhFunTy t1 t2)

mkLPhAppTy :: LPhType name -> LPhType name -> LPhType name
mkLPhAppTy t1@(Located span1 _) t2@(Located span2 _) =
  Located (combineSrcSpans span1 span2) (PhAppTy t1 t2)
