module Compiler.PhSyn.PhSyn where

import Data.Text (Text)

import Compiler.BasicTypes.Location
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType
import Prettyprinter
import Utils.Output

data PhModule a = Module
  { nodeID :: NodeID
  , modName :: Maybe Text
  , modDecls :: [PhDecl a]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PhDecl id
  = Binding NodeID (PhBind id)
  | Signature NodeID (Sig id)
  | DataDecl NodeID id [id] [ConDecl id]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

isBinding :: PhDecl id -> Bool
isBinding (Binding _ _) = True
isBinding _ = False

isSignature :: PhDecl id -> Bool
isSignature (Signature _ _) = True
isSignature _ = False

-- This will become more interesting when we add records
data ConDecl id
  = ConDecl NodeID id [PhType id]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty b) => Pretty (PhModule b) where
  pretty (Module _ Nothing decls) = vcat (map pretty decls)
  pretty (Module _ (Just name) decls) =
    vcat $
      ("module" <+> pretty name <+> "where")
        : map pretty decls

instance (Pretty id) => Pretty (PhDecl id) where
  pretty (Binding _ binding) = pretty binding
  pretty (Signature _ sig) = pretty sig
  pretty (DataDecl _ name tyvars (c : cs)) =
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
  pretty (ConDecl _ name argTypes) = pretty name <+> hsep (map pretty argTypes)
