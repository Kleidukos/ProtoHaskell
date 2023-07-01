module Compiler.PhSyn.PhExpr where

import Data.Text (Text)

import Compiler.BasicTypes.SrcLoc
import Compiler.PhSyn.PhType

import Prettyprinter
import Utils.Output

type LPhExpr id = Located (PhExpr id)
data PhExpr id
  = PhVar id
  | PhLit PhLit
  | PhLam (MatchGroup id)
  | PhApp (LPhExpr id) (LPhExpr id)
  | OpApp
      (LPhExpr id) -- left operand
      (LPhExpr id) -- operator, ALWAYS a PhVar
      (LPhExpr id) -- right operand
  | NegApp (LPhExpr id)
  | -- Parenthesized expr, see NOTE: [Par constructors in syn]
    PhPar (LPhExpr id)
  | PhCase (LPhExpr id) (MatchGroup id)
  | PhIf
      (LPhExpr id) -- predicate
      (LPhExpr id) -- consequent
      (LPhExpr id) -- alternative
  | PhLet (LPhLocalBinds id) (LPhExpr id)
  | PhDo [LStmt id]
  | ExplicitTuple [LPhTupArg id]
  | ExplicitList [LPhExpr id]
  | ArithSeq (ArithSeqInfo id)
  | Typed (LPhType id) (LPhExpr id)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

mkLPhAppExpr :: LPhExpr id -> LPhExpr id -> LPhExpr id
mkLPhAppExpr e1@(Located s1 _) e2@(Located s2 _) =
  Located (combineSrcSpans s1 s2) (PhApp e1 e2)

data PhLit
  = LitInt Integer
  | LitFloat Double
  | LitChar Char
  | LitString Text
  deriving (Eq, Ord, Show)

data MatchGroup id = MG
  { alternatives :: [LMatch id]
  , context :: MatchContext
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type LMatch id = Located (Match id)
data Match id = Match
  { matchPats :: [Located id]
  , rhs :: LRHS id
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type LRHS id = Located (RHS id)
data RHS id = RHS
  { expr :: LPhExpr id
  , localBinds :: LPhLocalBinds id -- where clause
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
-- have one GRHS per guard, which is shaped like GRHS [LGuardStmt] LPhExpr
-- where LGuardStmt is just an LStmt (same as statements in a do-block, meaning distinguished
-- by the context). Then RHS contains a [LGRHS] instead of just an LGRHS.
type LGuard id = Located (Guard id)
data Guard id = Guard (LPhExpr id) (LPhExpr id)
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

-- type LPat id = Located (Pat id)
--
-- -- TODO: constructor for infix pattern, like x:xs or gexpr `Guard` body
-- -- Alternatively we could just let that fall under PCon.
-- data Pat id
--   = PVar id
--   | PCon id [Pat id]
--   | PAs id (Pat id)
--   | PLit PhLit
--   | PWild
--   | PTuple [Pat id]
--   | PList [Pat id]
--   | ParPat (Pat id) -- Parenthesized pattern, see NOTE: [Par constructors in syn]
--   deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type LPhLocalBinds id = Located (PhLocalBinds id)
data PhLocalBinds id = LocalBinds [LPhBind id] [LSig id]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type LPhBind id = Located (PhBind id)
data PhBind id
  = -- | f x = e
    -- id = f, mg = ([PVar x], body = b)
    FunBind id (MatchGroup id)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type LStmt id = Located (Stmt id)
data Stmt id
  = -- | exp
    SExpr (LPhExpr id)
  | -- \| let bindings

    -- | -- | pat <- exp
    --   SGenerator (LPat id) (LPhExpr id)
    SLet (LPhLocalBinds id)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- This will become more interesting if we implement TupleSections
type LPhTupArg id = LPhExpr id

data ArithSeqInfo id
  = From (LPhExpr id)
  | FromThen
      (LPhExpr id)
      (LPhExpr id)
  | FromTo
      (LPhExpr id)
      (LPhExpr id)
  | FromThenTo
      (LPhExpr id)
      (LPhExpr id)
      (LPhExpr id)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{- NOTE: [Par constructors in syn]

To reduce headache:

\* The pretty printer does NOT add parens, except for PhPar.

In general when printing back what the user wrote, we will use locations and
print directly from the source code. But when pretty printing generated expressions,
we simply ensure that we generate the correct PhPar wrappers.

-}

type LSig id = Located (Sig id)
data Sig id
  = TypeSig id (LPhType id)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Assoc = Infix | InfixL | InfixR deriving (Eq, Ord, Show, Enum, Bounded)

----------------------------------------------------------------------------------
-- Pretty instances
----------------------------------------------------------------------------------

instance (Pretty id) => Pretty (PhExpr id) where
  pretty (PhVar id) = pretty id
  pretty (PhLit lit) = pretty lit
  pretty (PhLam mg) = pretty mg
  pretty (PhApp e1 e2) = asPrefixVar (pretty e1) <+> asPrefixVar (pretty e2)
  pretty (OpApp e1 e2 e3) =
    asPrefixVar (pretty e1)
      <+> asInfixVar (pretty e2)
      <+> asPrefixVar (pretty e3)
  pretty (NegApp e) = pretty @Text "-" <> pretty e
  pretty (PhPar e) = parens $ pretty e
  pretty (PhCase scrut mg) =
    pretty @Text "case"
      <+> pretty scrut
      <+> pretty @Text "of"
      <+> indent 2 (pretty mg)
  pretty (PhIf p t f) =
    pretty @Text "if"
      <+> pretty p
      <+> pretty @Text "then"
      <+> pretty t
      <+> pretty @Text "else"
      <+> pretty f
  pretty (PhLet binds e) =
    pretty @Text "let"
      <+> align (pretty binds)
      <+> pretty @Text "in"
      <+> pretty e
  pretty (PhDo stmts) = pretty @Text "do" <+> align (vcat $ map pretty stmts)
  pretty (ExplicitTuple tupArgs) = parens $ hcat $ punctuate comma $ map pretty tupArgs
  pretty (ExplicitList elems) = brackets $ hsep $ punctuate comma $ map pretty elems
  pretty (ArithSeq info) = brackets $ pretty info
  pretty (Typed t e) = pretty e <+> dcolon <+> pretty t

instance Pretty PhLit where
  pretty (LitInt i) = pretty i
  pretty (LitFloat d) = pretty d
  pretty (LitChar c) = pretty '\'' <> pretty c <> pretty '\''
  pretty (LitString s) = pretty $ show s -- need show to do escaping

instance (Pretty id) => Pretty (MatchGroup id) where
  pretty (MG (map unLoc -> alts) ctxt) = vcat $ map (prettyMatch ctxt) alts

prettyMatch :: (Pretty id) => MatchContext -> Match id -> Doc ann
prettyMatch ctxt (Match pats rhs) =
  prettyWhen (ctxt == LamCtxt) backslash
    <> hsep (map pretty pats)
    <+> prettyRhs (if isCaseOrLamCtxt ctxt then arrow else equals) rhs

prettyLocals :: (Pretty id) => LPhLocalBinds id -> Doc ann
prettyLocals (unLoc -> LocalBinds [] []) = mempty
prettyLocals (unLoc -> ls) = pretty @Text "where" <+> indent 2 (pretty ls)

prettyRhs :: (Pretty id) => Doc ann -> LRHS id -> Doc ann
prettyRhs ctxt (unLoc -> RHS grhs locals) = attachLocals $ prettyGrhs ctxt grhs
  where
    attachLocals
      | (unLoc -> LocalBinds [] []) <- locals = id
      | otherwise = (<+> prettyLocals locals)

prettyGrhs :: (Pretty id) => Doc ann -> LPhExpr id -> Doc ann
prettyGrhs ctxt (unLoc -> body) = ctxt <+> pretty body

-- prettyGrhs ctxt (unLoc -> Guarded guards) = vcat $ map (prettyGuard ctxt) guards

prettyGuard :: (Pretty id) => Doc ann -> LGuard id -> Doc ann
prettyGuard ctxt (unLoc -> Guard guard body) = pipe <+> pretty guard <+> ctxt <+> pretty body

-- instance (Pretty id) => Pretty (Pat id) where
--   pretty (PVar id) = asPrefixVar (pretty id)
--   pretty (PCon id args) = asPrefixVar (pretty id) <+> hsep (map pretty args)
--   pretty (PAs id pat) = asPrefixVar (pretty id) <> pretty '@' <> pretty pat
--   pretty (PLit lit) = pretty lit
--   pretty PWild = pretty '_'
--   pretty (PTuple ps) = parens $ fsep $ punctuate comma $ map pretty ps
--   pretty (PList ps) = brackets . fsep . punctuate comma $ map pretty ps
--   pretty (ParPat pat) = parens $ pretty pat

instance (Pretty id) => Pretty (PhLocalBinds id) where
  pretty (LocalBinds binds sigs) = vcat (map pretty binds ++ map pretty sigs)

instance (Pretty id) => Pretty (PhBind id) where
  pretty (FunBind id mg) = pretty id <+> pretty mg

-- pretty (PatBind pat body) = pretty pat <+> prettyRhs (text "=") body

instance (Pretty id) => Pretty (Stmt id) where
  pretty (SExpr e) = pretty e
  -- pretty (SGenerator pat e) = pretty pat <+> larrow <+> pretty e
  pretty (SLet binds) = pretty @Text "let" <+> align (pretty binds)

instance (Pretty id) => Pretty (ArithSeqInfo id) where
  pretty (From e) = pretty e <+> pretty @Text ".."
  pretty (FromThen e1 e2) = (pretty e1 <> comma) <+> pretty e2 <+> pretty @Text ".."
  pretty (FromTo e1 e2) = pretty e1 <+> pretty @Text ".." <+> pretty e2
  pretty (FromThenTo e1 e2 e3) =
    (pretty e1 <> comma) <+> pretty e2 <+> pretty @Text ".." <+> pretty e3

instance (Pretty id) => Pretty (Sig id) where
  pretty (TypeSig name t) = pretty name <+> pretty @Text "::" <+> pretty t

-- pretty (FixitySig assoc prec ids) =
--   pretty assoc
--     <+> int prec
--     <+> hsep (punctuate comma $ map pretty ids)

instance Pretty Assoc where
  pretty Infix = pretty @Text "infix"
  pretty InfixL = pretty @Text "infixl"
  pretty InfixR = pretty @Text "infixr"
