module Compiler.PhSyn.PhSyn where

import Data.Text (Text)

import Compiler.BasicTypes.SrcLoc (Located, unLoc)

import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType
import Prettyprinter

data PhModule a = Module
  { modName :: Maybe (Located Text)
  , modDecls :: [LPhDecl a]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type LPhDecl a = Located (PhDecl a)
data PhDecl id
  = Binding (PhBind id)
  | Signature (Sig id)
  | DataDecl id [id] [ConDecl id]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- This will become more interesting when we add records
data ConDecl id
  = ConDecl id [PhType id]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty b) => Pretty (PhModule b) where
  pretty (Module Nothing decls) = vcat (map pretty decls)
  pretty (Module (Just name) decls) =
    vcat $
      ("module" <+> pretty (unLoc name) <+> "where")
        : map pretty decls

instance (Pretty id) => Pretty (PhDecl id) where
  pretty (Binding binding) = pretty binding
  pretty (Signature sig) = pretty sig
  pretty (DataDecl name tyvars (c : cs)) =
    "data"
      <+> pretty name
      <+> hsep (map pretty tyvars)
      <+> indent 2 (vcat cons)
    where
      cons :: [Doc ann]
      cons = (equals <+> pretty c) : prepend pipe (map pretty cs)
      prepend :: Doc ann -> [Doc ann] -> [Doc ann]
      prepend d = map (d <+>)

-- pretty (ClassDecl scs name tyvar binds) =
--   let pscs = case scs of
--         [] -> mempty
--         [sc] -> pretty sc <+> darrow
--         scs -> (parens . hsep . punctuate comma $ map pretty scs) <+> darrow
--    in pretty "class"
--         <+> pscs
--         <+> pretty name
--         <+> pretty tyvar
--         <+> pretty "where"
--         <+> indent 2 (pretty binds)
-- pretty (InstDecl prds name head binds) =
--   let prettyds = case prds of
--         [] -> mempty
--         [prd] -> pretty prd <+> darrow
--         prds -> (parens . hsep . punctuate comma $ map pretty prds) <+> darrow
--    in pretty "instance"
--         <+> prettyds
--         <+> pretty name
--         <+> pretty head
--         <+> pretty "where"
--         <+> indent 2 (pretty binds)

instance (Pretty id) => Pretty (ConDecl id) where
  pretty (ConDecl name argTypes) = pretty name <+> hsep (map pretty argTypes)
