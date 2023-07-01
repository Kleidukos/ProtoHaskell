module Compiler.PhSyn.PhType where

import Compiler.BasicTypes.SrcLoc
import Prettyprinter
import Data.Text (Text)

type LPhType id = Located (PhType id)
data PhType id
  = -- Type variable or type constructor
    PhVarTy id
  | PhBuiltInTyCon BuiltInTyCon -- Built in type constructors: (), [], (->), (,), (,,) ...
  | -- Context => type
    PhQualTy
      [Pred id] -- context (C in C => A)
      (LPhType id) -- payload (A in C => A)
  | PhAppTy (LPhType id) (LPhType id)
  | PhFunTy (LPhType id) (LPhType id)
  | PhListTy (LPhType id)
  | PhTupleTy [LPhType id]
  | PhParTy (LPhType id) -- parenthesized type
  -- See NOTE: [Par constructors in syn] in Compiler/PhSyn/PhExpr
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data BuiltInTyCon
  = UnitTyCon
  | ListTyCon
  | FunTyCon
  | TupleTyCon Int -- Int is the number of fields; so it should be >= 2
  deriving (Eq, Ord, Show)

data Pred id = IsIn id (PhType id) -- Eq a, Eq [a] etc.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty id) => Pretty (PhType id) where
  pretty (PhVarTy id) = pretty id
  pretty (PhBuiltInTyCon tycon) = pretty tycon
  pretty (PhQualTy ctxt t) = case ctxt of
    [] -> pretty t
    [c] -> pretty c <+> pretty @Text "=>" <+> pretty t
    cs -> parens (sep (punctuate comma $ map pretty cs)) <+> pretty @Text "=>" <+> pretty t
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

instance (Pretty id) => Pretty (Pred id) where
  pretty (IsIn id t) = pretty id <+> pretty t

mkLPhFunTy :: LPhType id -> LPhType id -> LPhType id
mkLPhFunTy t1@(Located span1 _) t2@(Located span2 _) =
  Located (combineSrcSpans span1 span2) (PhFunTy t1 t2)

mkLPhAppTy :: LPhType id -> LPhType id -> LPhType id
mkLPhAppTy t1@(Located span1 _) t2@(Located span2 _) =
  Located (combineSrcSpans span1 span2) (PhAppTy t1 t2)
