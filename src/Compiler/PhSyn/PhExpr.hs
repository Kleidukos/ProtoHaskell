module Compiler.PhSyn.PhExpr where

import Data.Text (Text)

import Compiler.PhSyn.PhType

import Compiler.BasicTypes.Location
import GHC.Records
import Prettyprinter
import Utils.Output

data PhExpr name
  = PhVar NodeID name
  | PhLit NodeID PhLit
  | PhLam NodeID (MatchGroup name)
  | PhApp NodeID (PhExpr name) (PhExpr name)
  | OpApp
      NodeID
      (PhExpr name) -- left operand
      (PhExpr name) -- operator, ALWAYS a PhVar
      (PhExpr name) -- right operand
  | NegApp NodeID (PhExpr name)
  | -- Parenthesized expr, see NOTE: [Par constructors in syn]
    PhPar NodeID (PhExpr name)
  | PhCase NodeID (PhExpr name) (MatchGroup name)
  | PhIf
      NodeID
      (PhExpr name) -- predicate
      (PhExpr name) -- consequent
      (PhExpr name) -- alternative
  | PhLet NodeID (PhLocalBinds name) (PhExpr name)
  | PhDo NodeID [Stmt name]
  | ExplicitTuple NodeID [PhTupArg name]
  | ExplicitList NodeID [PhExpr name]
  | ArithSeq NodeID (ArithSeqInfo name)
  | Typed NodeID (PhType name) (PhExpr name)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- mkPhAppExpr :: PhExpr name -> PhExpr name -> PhExpr name
-- mkPhAppExpr e1@(Located s1 _) e2@(Located s2 _) =
--   Located (combineSrcSpans s1 s2) (PhApp e1 e2)

data PhLit
  = LitInt Integer
  | LitFloat Double
  | LitChar Char
  | LitString Text
  deriving (Eq, Ord, Show)

data MatchGroup name = MG
  { nodeID :: NodeID
  , alternatives :: [Match name]
  , context :: MatchContext
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Match name = Match
  { nodeID :: NodeID
  , matchPats :: [Pat name]
  , rhs :: RHS name
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data RHS name = RHS
  { nodeID :: NodeID
  , expr :: PhExpr name
  , localBinds :: PhLocalBinds name -- where clause
  -- There is technically a distinction between "no local bindings" and
  -- "empty local bindings", syntactically, but semantically they are the same.
  -- So the pretty-printer won't print them. Messages printed from source would
  -- still (obviously) see the empty local bindings.
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{- NOTE: [GRHS]
Called "guarded right hand sides" even if there are actually no guards

Why do we need this thing? Why can't we simply unpack RHS into Match?
The short answer is that pattern bindings (see PhSyn.hs) need a PhLocalBinds too!
Life is easier if we simply put them directly in the RHS and handle both ways of entering
a GRHS uniformly later.
-}

-- General plan for PatternGuards in the future: instead of unguarded/guarded distinction,
-- have one GRHS per guard, which is shaped like GRHS [LGuardStmt] PhExpr
-- where LGuardStmt is just an LStmt (same as statements in a do-block, meaning distinguished
-- by the context). Then RHS contains a [LGRHS] instead of just an LGRHS.
data Guard name = Guard (PhExpr name) (PhExpr name)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data MatchContext
  = FunCtxt
  | CaseCtxt
  | LamCtxt
  | LetCtxt
  deriving (Eq, Ord, Show, Enum, Bounded)

isCaseOrLamCtxt :: MatchContext -> Bool
isCaseOrLamCtxt CaseCtxt = True
isCaseOrLamCtxt LamCtxt = True
isCaseOrLamCtxt _ = False

--
-- -- TODO: constructor for infix pattern, like x:xs or gexpr `Guard` body
-- -- Alternatively we could just let that fall under PCon.
data Pat name
  = PVar NodeID name
  | ParPat NodeID (Pat name) -- Parenthesized pattern, see NOTE: [Par constructors in syn]
  | PCon NodeID name [Pat name]
  | PTuple NodeID [Pat name]
  | PLit NodeID PhLit
  | PWildCard NodeID
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PhLocalBinds name
  = LocalBinds
      NodeID
      [PhBind name]
      [Sig name]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PhBind name
  = -- | f x = e
    FunBind NodeID name (MatchGroup name)
  | -- | let Just x = …
    -- or foo = 3
    PatBind NodeID (Pat name) (RHS name)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasField "name" (PhBind name) (Maybe name) where
  getField (FunBind _ name _) = Just name
  getField (PatBind _ pat _) = getName pat

getName :: Pat name -> Maybe name
getName = \case
  PVar _ name -> Just name
  ParPat _ pat -> getName pat
  PCon _ name _ -> Just name
  PTuple _ _ -> Nothing
  PLit _ _ -> Nothing
  PWildCard _ -> Nothing

instance HasField "nodeID" (PhBind name) NodeID where
  getField (FunBind nodeID _ _) = nodeID
  getField (PatBind _ pat _) = getNodeID pat

getNodeID :: Pat name -> NodeID
getNodeID = \case
  PVar nodeID _ -> nodeID
  ParPat _ pat -> getNodeID pat
  PCon nodeID _ _ -> nodeID
  PTuple nodeID _ -> nodeID
  PLit nodeID _ -> nodeID
  PWildCard nodeID -> nodeID

data Stmt name
  = -- | exp
    SExpr (PhExpr name)
  | -- \| pat <- exp
    --   SGenerator (LPat name) (PhExpr name)

    -- | let bindings
    SLet (PhLocalBinds name)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- This will become more interesting if we implement TupleSections
type PhTupArg name = PhExpr name

data ArithSeqInfo name
  = From NodeID (PhExpr name)
  | FromThen
      NodeID
      (PhExpr name)
      (PhExpr name)
  | FromTo
      NodeID
      (PhExpr name)
      (PhExpr name)
  | FromThenTo
      NodeID
      (PhExpr name)
      (PhExpr name)
      (PhExpr name)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{- NOTE: [Par constructors in syn]

To reduce headache:

\* The pretty printer does NOT add parens, except for PhPar.

In general when printing back what the user wrote, we will use locations and
print directly from the source code. But when pretty printing generated expressions,
we simply ensure that we generate the correct PhPar wrappers.

-}

data Sig name
  = TypeSig NodeID name (PhType name)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasField "name" (Sig name) name where
  getField (TypeSig _ name _) = name

data Assoc = Infix | InfixL | InfixR deriving (Eq, Ord, Show, Enum, Bounded)

----------------------------------------------------------------------------------
-- Pretty instances
----------------------------------------------------------------------------------

instance (Pretty name) => Pretty (PhExpr name) where
  pretty (PhVar _ name) = pretty name
  pretty (PhLit _ lit) = pretty lit
  pretty (PhLam _ mg) = pretty mg
  pretty (PhApp _ e1 e2) = asPrefixVar (pretty e1) <+> asPrefixVar (pretty e2)
  pretty (OpApp _ e1 e2 e3) =
    asPrefixVar (pretty e1)
      <+> asInfixVar (pretty e2)
      <+> asPrefixVar (pretty e3)
  pretty (NegApp _ e) = "-" <> pretty e
  pretty (PhPar _ e) = parens $ pretty e
  pretty (PhCase _ scrut mg) =
    "case"
      <+> pretty scrut
      <+> "of"
      $+$ indent 2 (pretty mg)
  pretty (PhIf _ p t f) =
    "if"
      <+> pretty p
      <+> "then"
      <+> pretty t
      <+> "else"
      <+> pretty f
  pretty (PhLet _ binds e) =
    "let"
      <+> align (pretty binds)
      $+$ "in"
      <+> pretty e
  pretty (PhDo _ stmts) = "do" <+> align (vcat $ map pretty stmts)
  pretty (ExplicitTuple _ tupArgs) = parens $ hcat $ punctuate comma $ map pretty tupArgs
  pretty (ExplicitList _ elems) = brackets $ hsep $ punctuate comma $ map pretty elems
  pretty (ArithSeq _ info) = brackets $ pretty info
  pretty (Typed _ t e) = pretty e <+> dcolon <+> pretty t

instance Pretty PhLit where
  pretty (LitInt i) = pretty i
  pretty (LitFloat d) = pretty d
  pretty (LitChar c) = pretty '\'' <> pretty c <> pretty '\''
  pretty (LitString s) = pretty $ show s -- need show to do escaping

instance (Pretty name) => Pretty (MatchGroup name) where
  pretty (MG _ alternatives ctxt) = vcat $ map (prettyMatch ctxt) alternatives

instance (Pretty name) => Pretty (Pat name) where
  pretty (PVar _ nameent) = asPrefixVar (pretty nameent)
  pretty (PCon _ nameent args) = asPrefixVar (pretty nameent) <+> hsep (map pretty args)
  pretty (PTuple _ ps) = parens $ sep $ punctuate comma $ map pretty ps
  pretty (ParPat _ pat) = parens $ pretty pat
  pretty (PLit _ pat) = pretty pat
  pretty (PWildCard _) = "_"

prettyMatch :: (Pretty name) => MatchContext -> Match name -> Doc ann
prettyMatch ctxt (Match _ pats rhs) =
  prettyWhen (ctxt == LamCtxt) backslash
    <> hsep (map pretty pats)
    <+> prettyRhs (if isCaseOrLamCtxt ctxt then arrow else equals) rhs

prettyLocals :: (Pretty name) => PhLocalBinds name -> Doc ann
prettyLocals (LocalBinds _ [] []) = mempty
prettyLocals ls = "where" $+$ indent 2 (pretty ls)

prettyRhs :: (Pretty name) => Doc ann -> RHS name -> Doc ann
prettyRhs ctxt (RHS _ grhs locals) = attachLocals $ prettyGrhs ctxt grhs
  where
    attachLocals
      | (LocalBinds _ [] []) <- locals = id
      | otherwise = ($+$ prettyLocals locals)

prettyGrhs :: (Pretty name) => Doc ann -> PhExpr name -> Doc ann
prettyGrhs ctxt body = ctxt <+> pretty body

-- prettyGrhs ctxt (Guarded guards) = vcat $ map (prettyGuard ctxt) guards

prettyGuard :: (Pretty name) => Doc ann -> Guard name -> Doc ann
prettyGuard ctxt (Guard guard body) = pipe <+> pretty guard <+> ctxt <+> pretty body

instance (Pretty name) => Pretty (PhLocalBinds name) where
  pretty (LocalBinds _ binds sigs) = vcat (map pretty binds ++ map pretty sigs)

instance (Pretty name) => Pretty (PhBind name) where
  pretty (FunBind _ name mg) = pretty name <+> pretty mg
  pretty (PatBind _ pat body) = pretty pat <+> prettyRhs "=" body

instance (Pretty name) => Pretty (Stmt name) where
  pretty (SExpr e) = pretty e
  -- pretty (SGenerator pat e) = pretty pat <+> larrow <+> pretty e
  pretty (SLet binds) = "let" <+> align (pretty binds)

instance (Pretty name) => Pretty (ArithSeqInfo name) where
  pretty (From _ e) = pretty e <+> ".."
  pretty (FromThen _ e1 e2) = pretty e1 <> comma <+> pretty e2 <+> ".."
  pretty (FromTo _ e1 e2) = pretty e1 <+> ".." <+> pretty e2
  pretty (FromThenTo _ e1 e2 e3) =
    pretty e1 <> comma <+> pretty e2 <+> ".." <+> pretty e3

instance (Pretty name) => Pretty (Sig name) where
  pretty (TypeSig _ name t) = pretty name <+> "::" <+> pretty t

-- pretty (FixitySig assoc prec names) =
--   pretty assoc
--     <+> int prec
--     <+> hsep (punctuate comma $ map pretty names)

instance Pretty Assoc where
  pretty Infix = "infix"
  pretty InfixL = "infixl"
  pretty InfixR = "infixr"
