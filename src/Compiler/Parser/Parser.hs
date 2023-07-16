module Compiler.Parser.Parser where

import Prelude hiding (lex)
import Data.Text (Text)
import Prettyprinter (Doc, pretty)
import Control.Applicative
import Control.Monad.State.Strict
import Text.Megaparsec (eof, sepBy1, try, (<?>))
import Text.Megaparsec.Error
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Internal

import Compiler.BasicTypes.Location
import Compiler.BasicTypes.ParsedName
import Compiler.Parser.Helpers
import Compiler.Parser.Lexer
import Compiler.Parser.Pattern qualified as Pattern
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhSyn
import Compiler.PhSyn.PhType

parse :: FilePath -> String -> IO (Either (Doc ann) (PhModule ParsedName))
parse srcname input = case lex srcname input of
  Left err -> pure $ Left (pretty err)
  Right lexemes -> do
    initState <- initPState srcname
    _ (runParserT (moduleParser <* eof) srcname (TokStream lexemes srcname)) initState >>= \case
      Right v -> pure $ Right v
      Left errBundle -> pure $ Left $ pretty $ errorBundlePretty errBundle

runParserT :: FilePath -> Parser a -> State PState (Maybe a, Maybe w)
runParserT path p = do
  let s = initPState path
  (Reply s' _ result) <- Megaparsec.runParsecT p s
  let toBundle es =
        ParseErrorBundle
          { bundleErrors = NE.sortWith Megaparsec.errorOffset es
          , bundlePosState = Megaparsec.statePosState s
          }
  pure $ case result of
    Error fatalError ->
      (Nothing, Just $ ParsingError $ toBundle $ fatalError :| Megaparsec.stateParseErrors s')
    OK _ x ->
      let nonFatalErrs = viaNonEmpty toBundle (Megaparsec.stateParseErrors s')
       in (Just x, ParsingError <$> nonFatalErrs)

moduleParser :: Parser (PhModule ParsedName)
moduleParser = withNodeID $ \nodeID ->
  Module nodeID
    <$> optional moduleHeader
    <*> parseTopDecls

moduleHeader :: Parser Text
moduleHeader = do
  reserved "module"
  name <- modlName
  reserved "where"
  pure name

parseTopDecls :: Parser [PhDecl ParsedName]
parseTopDecls = block parseTopDecl

parseTopDecl :: Parser (PhDecl ParsedName)
parseTopDecl = withNodeID $ \nodeID ->
  parseDataDecl nodeID
    <|> parseSignature nodeID
    <|> parseBinding nodeID
    <?> "top-level declaration"

parseDataDecl :: NodeID -> Parser (PhDecl ParsedName)
parseDataDecl nodeID = do
  reserved "data"
  typename <- tyconid
  typevars <- many tyvarid
  reservedOp "="
  condecls <- sepBy1 parseConDecl (reservedOp "|")
  pure $ DataDecl nodeID typename typevars condecls

parseConDecl :: Parser (ConDecl ParsedName)
parseConDecl = withNodeID $ \nodeID -> do
  name <- dataconid
  fields <- many parseAType -- don't want to use parseType, it will just see
  -- AppTy before many can turn it into a list
  pure $ ConDecl nodeID name fields

parseSignature :: NodeID -> Parser (PhDecl ParsedName)
parseSignature nodeID =
  fmap (Signature nodeID) $
    try parseTypeSignature

parseTypeSignature :: Parser (Sig ParsedName)
parseTypeSignature = withNodeID $ \nodeID -> do
  name <- varid
  reservedOp "::"
  TypeSig nodeID name <$> parseContextType

parseBinding :: NodeID -> Parser (PhDecl ParsedName)
parseBinding nodeID = fmap (Binding nodeID) parseBind

parseBind :: Parser (PhBind ParsedName)
parseBind = try parseFunBinding <|> parsePatternBinding

-- TODO: parse bindings for operators
parseFunBinding :: Parser (PhBind ParsedName)
parseFunBinding = withNodeID $ \nodeID -> do
  funName <- varid
  match <- parseMatch FunCtxt
  pure $ FunBind nodeID funName $ MG{nodeID, alternatives = [match], context = FunCtxt}

parsePatternBinding :: Parser (PhBind ParsedName)
parsePatternBinding = withNodeID $ \nodeID ->
  PatBind nodeID <$> Pattern.parse <*> parseRHS LetCtxt

-- | Parses a single match which will eventually be part of a match group.
--
-- Considers the context as follows:
--
-- Lam contexts are not allowed to contain local bindings.
--
-- See also: 'parseRHS'
parseMatch :: MatchContext -> Parser (Match ParsedName)
parseMatch ctx = withNodeID $ \nodeID -> do
  pats <- many1 Pattern.parse
  rhs <- parseRHS ctx
  pure $ Match nodeID pats rhs

-- | Parses the right hand side of a binding.
-- Lam contexts are not allowed to contain local bindings. Case contexts /are/, though
-- hardly anyone ever uses that feature.
parseRHS :: MatchContext -> Parser (RHS ParsedName)
parseRHS ctx = withNodeID $ \nodeID -> do
  expr <- parseGRHS ctx
  mLocalBinds <-
    if ctx /= LamCtxt
      then optional (token TokWhere >> parseLocalBinds) <?> "where clause"
      else pure Nothing
  pure RHS{nodeID, expr, localBinds = flatten nodeID mLocalBinds}
  where
    flatten _nodeID (Just binds) = binds
    flatten nodeID Nothing = LocalBinds nodeID [] []

-- | Parses the "guarded right hand sides" of a binding. See NOTE: [GRHS] in PhExpr.
--
-- The left and right hand side are separated by '->' in Lam and Case contexts,
-- but by '=' in Fun and Let contexts
--
-- Lam contexts are not allowed to contain guards.
parseGRHS :: MatchContext -> Parser (PhExpr ParsedName)
parseGRHS LamCtxt = do
  matchCtx2Parser LamCtxt
  parseLocExpr
parseGRHS ctxt = parseUnguarded
  where
    parseUnguarded = matchCtx2Parser ctxt >> parseLocExpr

type LocalDecls = ([PhBind ParsedName], [Sig ParsedName])

parseLocalBinds :: Parser (PhLocalBinds ParsedName)
parseLocalBinds = withNodeID $ \nodeID -> do
  decls <- block parseLocalDecl
  (binds, sigs) <- pure $ separate decls
  pure $ LocalBinds nodeID binds sigs
  where
    -- The use of foldr is absolutely critical, as the order of the bindings matters.
    -- Folding from the right and prepending with (:) will maintain the order.
    -- The renamer should check the order *anyway* if the compiler is in debug mode.
    separate :: [PhDecl ParsedName] -> LocalDecls
    separate decls = foldr move ([], []) decls

    -- \| Takes the pair of signatures and bindings already parsed
    --  and parses a new one, placing it appropriately
    move :: PhDecl ParsedName -> LocalDecls -> LocalDecls
    move new (binds, sigs) = case new of
      Binding _nodeID' bind -> (bind : binds, sigs)
      Signature _nodeID' sig -> (binds, sig : sigs)

parseLocalDecl :: Parser (PhDecl ParsedName)
parseLocalDecl = withNodeID $ \nodeID ->
  parseSignature nodeID
    -- We'll delay rejecting pattern bindings in class
    -- or instance declarations for the renamer.
    <|> parseBinding nodeID
    <?> "local declaration"

matchCtx2Parser :: MatchContext -> Parser ()
matchCtx2Parser =
  void . \case
    FunCtxt -> reservedOp "=" <|> failArrowSign <?> eqpretty
    CaseCtxt -> reservedOp "->" <|> failEqualSign <?> arpretty
    LamCtxt -> reservedOp "->" <|> failEqualSign <?> arpretty
    LetCtxt -> reservedOp "=" <|> failArrowSign <?> eqpretty
  where
    failEqualSign = anticipateOp "=" arpretty
    failArrowSign = anticipateOp "->" eqpretty

    eqpretty = showTokenPretty TokEqual
    arpretty = showTokenPretty TokRArrow

-----------------------------------------------------------------------------------------
-- Parsing Expressions
-----------------------------------------------------------------------------------------

parseExpr :: Parser (PhExpr ParsedName)
parseExpr = withNodeID $ \nodeID ->
  do
    expression <- parseInfixExp
    mCType <- optional $ reservedOp "::" >> parseContextType
    pure $ case mCType of
      Nothing -> expression
      Just ty -> Typed nodeID ty expression
    <?> "expression"

parseLocExpr :: Parser (PhExpr ParsedName)
parseLocExpr = parseExpr

-- | Parses operator (infix) expressions.
-- We parse all operators as left associative and highest
-- precedence, then rebalance the tree later.
parseInfixExp :: Parser (PhExpr ParsedName)
parseInfixExp = withNodeID $ \nodeID ->
  parseSyntacticNegation nodeID
    <|> parseBExp `chainl1` (mkOpApp nodeID <$> parseQop)

mkOpApp
  :: NodeID
  -> PhExpr ParsedName
  -> PhExpr ParsedName
  -> PhExpr ParsedName
  -> PhExpr ParsedName
mkOpApp nodeID op left = OpApp nodeID left op

parseQop :: Parser (PhExpr ParsedName)
parseQop = withNodeID $ \nodeID ->
  PhVar nodeID <$> (varsym <|> backticks varid) <?> "operator"

parseSyntacticNegation :: NodeID -> Parser (PhExpr ParsedName)
parseSyntacticNegation nodeID = do
  satisfy isDashSym
  NegApp nodeID <$> parseInfixExp
  where
    isDashSym :: Token -> Bool
    isDashSym (TokVarSym "-") = True
    isDashSym _ = False

parseBExp :: Parser (PhExpr ParsedName)
parseBExp =
  parseLamExp
    <|> parseLetExp
    <|> parseIfExp
    <|> parseCaseExp
    <|> parseDoExp
    <|> parseFExp

parseLamExp :: Parser (PhExpr ParsedName)
parseLamExp = withNodeID $ \nodeID1 -> do
  reservedOp "\\"
  match <- parseMatch LamCtxt -- parseMatch parses RHS
  withNodeID $ \nodeID2 ->
    pure $
      PhLam nodeID1 $
        MG nodeID2 [match] LamCtxt

parseLetExp :: Parser (PhExpr ParsedName)
parseLetExp = withNodeID $ \nodeID -> do
  reserved "let"
  decls <- parseLocalBinds
  reserved "in"
  PhLet nodeID decls <$> parseLocExpr

parseIfExp :: Parser (PhExpr ParsedName)
parseIfExp = withNodeID $ \nodeID -> do
  reserved "if"
  c <- parseLocExpr
  optSemi
  maybeAlign >> reserved "then"
  t <- parseLocExpr
  optSemi
  maybeAlign >> reserved "else"
  PhIf nodeID c t <$> parseLocExpr

parseCaseExp :: Parser (PhExpr ParsedName)
parseCaseExp = withNodeID $ \nodeID -> do
  reserved "case"
  scrut <- parseLocExpr
  reserved "of"
  PhCase nodeID scrut <$> parseCaseAlts

parseDoExp :: Parser (PhExpr ParsedName)
parseDoExp = withNodeID $ \nodeID -> do
  reserved "do"
  PhDo nodeID <$> parseDoStmts

parseFExp :: Parser (PhExpr ParsedName)
parseFExp = withNodeID parseAExp

parseAExp :: NodeID -> Parser (PhExpr ParsedName)
parseAExp nodeID =
  PhVar nodeID <$> var
    <|> parseGCon nodeID
    <|> parseLiteral nodeID
    <|> parens parseExprParen
    <|> brackets parseExprBracket
    <?> "term"

parseGCon :: NodeID -> Parser (PhExpr ParsedName)
parseGCon nodeID = PhVar nodeID <$> dataconid

parseLiteral :: NodeID -> Parser (PhExpr ParsedName)
parseLiteral nodeID =
  (PhLit nodeID <$>) $
    LitInt <$> integer
      <|> LitFloat <$> float
      <|> LitChar <$> charLiteral
      <|> LitString <$> stringLiteral

parseExprParen :: Parser (PhExpr ParsedName)
parseExprParen = withNodeID $ \nodeID -> do
  exps <- commaSep1 parseLocExpr
  pure $ case exps of
    -- () is a GCon
    [expression] -> PhPar nodeID expression
    expressions -> ExplicitTuple nodeID expressions

parseExprBracket :: Parser (PhExpr ParsedName)
parseExprBracket = withNodeID $ \nodeID -> do
  exps <- commaSep1 parseExpr
  case exps of
    [expression] -> parseDotsSeq expression <|> pure (ExplicitList nodeID [expression])
    [e1, e2] -> parseCommaSeq e1 e2 <|> pure (ExplicitList nodeID [e1, e2])
    _ -> pure $ ExplicitList nodeID exps

-- | Given the first two elements of an arithmetic sequence [e1, e2 ..] or [e1, e2 .. e3]
-- parse the rest.
parseCommaSeq :: PhExpr ParsedName -> PhExpr ParsedName -> Parser (PhExpr ParsedName)
parseCommaSeq e1 e2 = withNodeID $ \nodeID ->
  ArithSeq nodeID <$> do
    reservedOp ".."
    e3 <- optional parseLocExpr
    withNodeID $ \nodeID2 -> do
      case e3 of
        Nothing -> pure $ FromThen nodeID2 e1 e2
        Just e -> pure $ FromThenTo nodeID2 e1 e2 e

parseDotsSeq :: PhExpr ParsedName -> Parser (PhExpr ParsedName)
parseDotsSeq expression = withNodeID $ \nodeID ->
  ArithSeq nodeID <$> do
    reservedOp ".."
    e2 <- optional parseLocExpr
    withNodeID $ \nodeID2 -> do
      case e2 of
        Nothing -> pure $ From nodeID2 expression
        Just e -> pure $ FromTo nodeID2 expression e

parseDoStmts :: Parser [Stmt ParsedName]
parseDoStmts = block1 parseDoStmt

parseDoStmt :: Parser (Stmt ParsedName)
parseDoStmt =
  try (reserved "let" *> (SLet <$> parseLocalBinds))
    <|> SExpr <$> parseLocExpr
    <?> "statement of a do block"

parseCaseAlts :: Parser (MatchGroup ParsedName)
parseCaseAlts = withNodeID $ \nodeID -> MG nodeID <$> block1 (parseMatch CaseCtxt) <*> pure CaseCtxt

-----------------------------------------------------------------------------------------
-- Parsing Types
-----------------------------------------------------------------------------------------

-- | Parses a type with context, like Eq a => a -> a -> Bool
parseContextType :: Parser (PhType ParsedName)
parseContextType = parseType

parseType :: Parser (PhType ParsedName)
parseType = withNodeID $ \nodeID -> do
  btype <- parseBType
  funtypes <- many (reservedOp "->" >> parseType)
  pure $ foldl (PhFunTy nodeID) btype funtypes

parseBType :: Parser (PhType ParsedName)
parseBType = withNodeID $ \nodeID -> do
  atypes <- many1 parseAType
  pure $ foldl1 (PhAppTy nodeID) atypes

parseAType :: Parser (PhType ParsedName)
parseAType = withNodeID $ \nodeID ->
  PhVarTy nodeID <$> tyvarid
    <|> try parseGTyCon
    <|> do
      types <- parens $ commaSep1 parseType
      case types of
        -- () is a GTyCon
        [t] -> pure $ PhParTy nodeID t
        ts -> pure $ PhTupleTy nodeID ts
    <|> PhListTy nodeID <$> brackets parseType

parseGTyCon :: Parser (PhType ParsedName)
parseGTyCon = withNodeID $ \nodeID ->
  PhVarTy nodeID <$> tyconid
    <|> brackets (pure ()) $> PhBuiltInTyCon nodeID ListTyCon
    <|> try (parens (reservedOp "->") $> PhBuiltInTyCon nodeID FunTyCon)
    <|> do
      len <- length <$> parens (many comma)
      pure $ case len of
        0 -> PhBuiltInTyCon nodeID UnitTyCon
        _ -> PhBuiltInTyCon nodeID (TupleTyCon $ len + 1)
