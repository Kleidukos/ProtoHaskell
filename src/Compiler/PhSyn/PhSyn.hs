module Compiler.PhSyn.PhSyn where

import Data.Text (Text)

import Compiler.BasicTypes.SrcLoc (Located, unLoc)

import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType
import Prettyprinter
import Utils.Output

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

isBinding :: PhDecl id -> Bool
isBinding (Binding _) = True
isBinding _ = False

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
      $+$ indent 2 (vcat cons)
    where
      cons :: [Doc ann]
      cons = (equals <+> pretty c) : prepend pipe (map pretty cs)
      prepend :: Doc ann -> [Doc ann] -> [Doc ann]
      prepend d = map (d <+>)

instance (Pretty id) => Pretty (ConDecl id) where
  pretty (ConDecl name argTypes) = pretty name <+> hsep (map pretty argTypes)
