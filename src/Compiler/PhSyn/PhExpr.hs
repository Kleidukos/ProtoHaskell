module Compiler.PhSyn.PhExpr where

import Data.Text (Text)

import Compiler.BasicTypes.SrcLoc
import Compiler.PhSyn.PhType

import GHC.Records
import Prettyprinter
import Utils.Output
import Compiler.BasicTypes.Name

type LPhExpr name = Located (PhExpr name)
data PhExpr name
  = PhVar name
  | PhLit PhLit
  | PhLam (MatchGroup name)
  | PhApp (LPhExpr name) (LPhExpr name)
  | OpApp
      (LPhExpr name) -- left operand
      (LPhExpr name) -- operator, ALWAYS a PhVar
      (LPhExpr name) -- right operand
  | NegApp (LPhExpr name)
  | -- Parenthesized expr, see NOTE: [Par constructors in syn]
    PhPar (LPhExpr name)
  | PhCase (LPhExpr name) (MatchGroup name)
  | PhIf
      (LPhExpr name) -- predicate
      (LPhExpr name) -- consequent
      (LPhExpr name) -- alternative
  | PhLet (LPhLocalBinds name) (LPhExpr name)
  | PhDo [LStmt name]
  | ExplicitTuple [LPhTupArg name]
  | ExplicitList [LPhExpr name]
  | ArithSeq (ArithSeqInfo name)
  | Typed (LPhType name) (LPhExpr name)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data TypedExpr = TypedExpr (PhType Name) (PhExpr Name)
  deriving stock (Eq, Ord, Show)

mkLPhAppExpr :: LPhExpr name -> LPhExpr name -> LPhExpr name
mkLPhAppExpr e1@(Located s1 _) e2@(Located s2 _) =
  Located (combineSrcSpans s1 s2) (PhApp e1 e2)

data PhLit
  = LitInt Integer
  | LitFloat Double
  | LitChar Char
  | LitString Text
  deriving stock (Eq, Ord, Show)

data MatchGroup name = MG
  { alternatives :: [LMatch name]
  , context :: MatchContext
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type LMatch name = Located (Match name)
data Match name = Match
  { matchPats :: [LPat name]
  , rhs :: LRHS name
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type LRHS name = Located (RHS name)
data RHS name = RHS
  { expr :: LPhExpr name
  , localBinds :: LPhLocalBinds name -- where clause
  -- There is technically a distinction between "no local bindings" and
  -- "empty local bindings", syntactically, but semantically they are the same.
  -- So the pretty-printer won't print them. Messages printed from source would
  -- still (obviously) see the empty local bindings.
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

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
type LGuard name = Located (Guard name)
data Guard name = Guard (LPhExpr name) (LPhExpr name)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data MatchContext
  = FunCtxt
  | CaseCtxt
  | LamCtxt
  | LetCtxt
  deriving stock (Eq, Ord, Show, Enum, Bounded)

isCaseOrLamCtxt :: MatchContext -> Bool
isCaseOrLamCtxt CaseCtxt = True
isCaseOrLamCtxt LamCtxt = True
isCaseOrLamCtxt _ = False

type LPat name = Located (Pat name)

--
-- -- TODO: constructor for infix pattern, like x:xs or gexpr `Guard` body
-- -- Alternatively we could just let that fall under PCon.
data Pat name
  = PVar name
  | ParPat (Pat name) -- Parenthesized pattern, see NOTE: [Par constructors in syn]
  | PCon name [Pat name]
  | PTuple [Pat name]
  | PLit PhLit
  | PWildCard
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type LPhLocalBinds name = Located (PhLocalBinds name)
data PhLocalBinds name = LocalBinds [LPhBind name] [LSig name]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type LPhBind name = Located (PhBind name)
data PhBind name
  = -- | f x = e
    FunBind name (MatchGroup name)
  | -- | let Just x = …
    -- or foo = 3
    PatBind (LPat name) (LRHS name)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasField "name" (PhBind name) (Maybe name) where
  getField (FunBind name _) = Just name
  getField (PatBind pat _) = getName $ unLoc pat

getName :: Pat name -> Maybe name
getName = \case
  PVar name -> Just name
  ParPat pat -> getName pat
  PCon name _ -> Just name
  PTuple _ -> Nothing
  PLit _ -> Nothing
  PWildCard -> Nothing

type LStmt name = Located (Stmt name)
data Stmt name
  = -- | exp
    SExpr (LPhExpr name)
  | -- \| pat <- exp
    --   SGenerator (LPat name) (LPhExpr name)

    -- | let bindings
    SLet (LPhLocalBinds name)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- This will become more interesting if we implement TupleSections
type LPhTupArg name = LPhExpr name

data ArithSeqInfo name
  = From (LPhExpr name)
  | FromThen
      (LPhExpr name)
      (LPhExpr name)
  | FromTo
      (LPhExpr name)
      (LPhExpr name)
  | FromThenTo
      (LPhExpr name)
      (LPhExpr name)
      (LPhExpr name)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

{- NOTE: [Par constructors in syn]

To reduce headache:

\* The pretty printer does NOT add parens, except for PhPar.

In general when printing back what the user wrote, we will use locations and
print directly from the source code. But when pretty printing generated expressions,
we simply ensure that we generate the correct PhPar wrappers.

-}

type LSig name = Located (Sig name)
data Sig name
  = TypeSig name (LPhType name)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasField "name" (Sig name) name where
  getField (TypeSig name _) = name

data Assoc = Infix | InfixL | InfixR deriving stock (Eq, Ord, Show, Enum, Bounded)

----------------------------------------------------------------------------------
-- Pretty instances
----------------------------------------------------------------------------------

instance (Pretty name) => Pretty (PhExpr name) where
  pretty (PhVar name) = pretty name
  pretty (PhLit lit) = pretty lit
  pretty (PhLam mg) = pretty mg
  pretty (PhApp e1 e2) = asPrefixVar (pretty e1) <+> asPrefixVar (pretty e2)
  pretty (OpApp e1 e2 e3) =
    asPrefixVar (pretty e1)
      <+> asInfixVar (pretty e2)
      <+> asPrefixVar (pretty e3)
  pretty (NegApp e) = "-" <> pretty e
  pretty (PhPar e) = parens $ pretty e
  pretty (PhCase scrut mg) =
    "case"
      <+> pretty scrut
      <+> "of"
      $+$ indent 2 (pretty mg)
  pretty (PhIf p t f) =
    "if"
      <+> pretty p
      <+> "then"
      <+> pretty t
      <+> "else"
      <+> pretty f
  pretty (PhLet binds e) =
    "let"
      <+> align (pretty binds)
      $+$ "in"
      <+> pretty e
  pretty (PhDo stmts) = "do" <+> align (vcat $ map pretty stmts)
  pretty (ExplicitTuple tupArgs) = parens $ hcat $ punctuate comma $ map pretty tupArgs
  pretty (ExplicitList elems) = brackets $ hsep $ punctuate comma $ map pretty elems
  pretty (ArithSeq info) = brackets $ pretty info
  pretty (Typed t e) = pretty e <+> dcolon <+> pretty t

instance Pretty PhLit where
  pretty (LitInt i) = pretty i
  pretty (LitFloat d) = pretty d
  pretty (LitChar c) = pretty '\'' <> pretty c <> pretty '\''
  pretty (LitString s) = pretty $ show s -- need show to do escaping

instance (Pretty name) => Pretty (MatchGroup name) where
  pretty (MG (map unLoc -> alts) ctxt) = vcat $ map (prettyMatch ctxt) alts

instance (Pretty name) => Pretty (Pat name) where
  pretty (PVar nameent) = asPrefixVar (pretty nameent)
  pretty (PCon nameent args) = asPrefixVar (pretty nameent) <+> hsep (map pretty args)
  pretty (PTuple ps) = parens $ sep $ punctuate comma $ map pretty ps
  pretty (ParPat pat) = parens $ pretty pat
  pretty (PLit pat) = pretty pat
  pretty PWildCard = "_"

prettyMatch :: (Pretty name) => MatchContext -> Match name -> Doc ann
prettyMatch ctxt (Match pats rhs) =
  prettyWhen (ctxt == LamCtxt) backslash
    <> hsep (map pretty pats)
    <+> prettyRhs (if isCaseOrLamCtxt ctxt then arrow else equals) rhs

prettyLocals :: (Pretty name) => LPhLocalBinds name -> Doc ann
prettyLocals (unLoc -> LocalBinds [] []) = mempty
prettyLocals (unLoc -> ls) = "where" $+$ indent 2 (pretty ls)

prettyRhs :: (Pretty name) => Doc ann -> LRHS name -> Doc ann
prettyRhs ctxt (unLoc -> RHS grhs locals) = attachLocals $ prettyGrhs ctxt grhs
  where
    attachLocals
      | (unLoc -> LocalBinds [] []) <- locals = id
      | otherwise = ($+$ prettyLocals locals)

prettyGrhs :: (Pretty name) => Doc ann -> LPhExpr name -> Doc ann
prettyGrhs ctxt (unLoc -> body) = ctxt <+> pretty body

-- prettyGrhs ctxt (unLoc -> Guarded guards) = vcat $ map (prettyGuard ctxt) guards

prettyGuard :: (Pretty name) => Doc ann -> LGuard name -> Doc ann
prettyGuard ctxt (unLoc -> Guard guard body) = pipe <+> pretty guard <+> ctxt <+> pretty body

instance (Pretty name) => Pretty (PhLocalBinds name) where
  pretty (LocalBinds binds sigs) = vcat (map pretty binds ++ map pretty sigs)

instance (Pretty name) => Pretty (PhBind name) where
  pretty (FunBind name mg) = pretty name <+> pretty mg
  pretty (PatBind pat body) = pretty pat <+> prettyRhs "=" body

instance (Pretty name) => Pretty (Stmt name) where
  pretty (SExpr e) = pretty e
  -- pretty (SGenerator pat e) = pretty pat <+> larrow <+> pretty e
  pretty (SLet binds) = "let" <+> align (pretty binds)

instance (Pretty name) => Pretty (ArithSeqInfo name) where
  pretty (From e) = pretty e <+> ".."
  pretty (FromThen e1 e2) = (pretty e1 <> comma) <+> pretty e2 <+> ".."
  pretty (FromTo e1 e2) = pretty e1 <+> ".." <+> pretty e2
  pretty (FromThenTo e1 e2 e3) =
    (pretty e1 <> comma) <+> pretty e2 <+> ".." <+> pretty e3

instance (Pretty name) => Pretty (Sig name) where
  pretty (TypeSig name t) = pretty name <+> "::" <+> pretty t

-- pretty (FixitySig assoc prec names) =
--   pretty assoc
--     <+> int prec
--     <+> hsep (punctuate comma $ map pretty names)

instance Pretty Assoc where
  pretty Infix = "infix"
  pretty InfixL = "infixl"
  pretty InfixR = "infixr"
