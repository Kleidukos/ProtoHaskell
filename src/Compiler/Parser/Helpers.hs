{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Compiler.Parser.Helpers where

import Control.Concurrent.Counter (Counter)
import Control.Concurrent.Counter qualified as Counter
import Control.Monad
import Control.Monad.State
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, isJust)
import Data.Proxy
import Data.Semigroup
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter hiding (align, comma)
import Prettyprinter.Render.String
import System.IO.Unsafe
import Text.Megaparsec hiding (State, Token, parse, satisfy, token)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug qualified as D
import Prelude hiding (lex)

-- import Debug.Trace

import Compiler.BasicTypes.FastString
import Compiler.BasicTypes.Location
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.SrcLoc
import Compiler.Parser.Lexer

data TokStream = TokStream
  { lexemes :: [Lexeme]
  , source :: String
  }
  deriving (Eq, Ord)

instance Show TokStream where
  show TokStream{lexemes} = show lexemes

instance Stream TokStream where
  type Token TokStream = Lexeme
  type Tokens TokStream = [Lexeme]

  tokensToChunk Proxy lexemes = lexemes
  chunkToTokens Proxy lexemes = lexemes
  chunkLength Proxy lexemes = length lexemes
  chunkEmpty Proxy lexemes = null lexemes
  take1_ s@TokStream{lexemes} = case lexemes of
    [] -> Nothing
    (l : ls) -> Just (l, s{lexemes = ls})
  takeN_ n s@TokStream{lexemes}
    | n <= 0 = Just ([], s)
    | null lexemes = Nothing
    | otherwise =
        let (xs, ls) = splitAt n lexemes
         in Just (xs, s{lexemes = ls})
  takeWhile_ p s@TokStream{lexemes} =
    let (xs, ls) = span p lexemes
     in (xs, s{lexemes = ls})

instance VisualStream TokStream where
  showTokens _proxy tokenStream =
    sconcat $
      NE.map
        ( \tok ->
            renderString $ layoutPretty defaultLayoutOptions $ pretty $ unLoc tok
        )
        tokenStream

instance TraversableStream TokStream where
  reachOffset o pst@PosState{..} =
    let rest = drop (o - pstateOffset) (lexemes pstateInput)
        nextPos = case rest of
          [] -> pstateSourcePos
          (Located _ TokIndent : (Located s _) : _) ->
            mkSrcPos $ srcSpanStart s
          (x : _) -> mkSrcPos $ srcSpanStart $ getLoc x
        currentLine = unPos (sourceLine nextPos) - 1
        cLineSrc = case lines (source pstateInput) of
          [] -> Nothing
          ls -> case ls !! currentLine of
            "" -> Nothing
            s -> Just s
     in (cLineSrc, pst{pstateInput = TokStream rest (source pstateInput)})

mkSrcPos :: SrcLoc -> SourcePos
mkSrcPos srcLoc =
  SourcePos
    (unpackFS $ unsafeLocFile srcLoc)
    (mkPos $ unsafeLocLine srcLoc)
    (mkPos $ unsafeLocCol srcLoc)

type Parser a = ParsecT CustomError TokStream (State PState) a

getFreshNodeID :: Parser NodeID
getFreshNodeID = do
  counter <- gets nodeIDSupply
  pure $ unsafePerformIO $ Counter.add counter 1

withNodeID :: (NodeID -> Parser a) -> Parser a
withNodeID p = do
  nodeID <- getFreshNodeID
  startPos <- getSourcePos
  let srcName = sourceName startPos
      startLine = sourceLine startPos
      startCol = sourceColumn startPos
      startLoc = mkSrcLoc srcName (unPos startLine) (unPos startCol)
  res <- p nodeID
  endPos <- gets endOfPrevToken
  modify $ \state -> state{locationMap = insertLocation nodeID (mkSrcSpan startLoc endPos) state.locationMap}
  pure res

data CustomError
  = PopEmptyLayoutCtx
  deriving (Eq, Ord, Show, Enum, Bounded)

instance ShowErrorComponent CustomError where
  showErrorComponent PopEmptyLayoutCtx =
    "Tried to pop a layout context, but there are no layout contexts"

data PState = PState
  { layout_ctx :: [LayoutContext]
  , indentOrd :: Ordering
  , endOfPrevToken :: SrcLoc
  , nodeIDSupply :: Counter
  , locationMap :: LocationMap
  }

data LayoutContext = Explicit | Implicit Pos deriving (Eq, Ord, Show)

initPState :: FilePath -> IO PState
initPState path = do
  nodeIDSupply <- Counter.new 0
  pure $ PState [] EQ noSrcLoc nodeIDSupply (LocationMap path mempty)

currentLayoutContext :: Parser (Maybe LayoutContext)
currentLayoutContext = gets ((\case [] -> Nothing; (x : _) -> Just x) . layout_ctx)

pushLayoutContext :: LayoutContext -> Parser ()
pushLayoutContext ctx =
  modify $ \s@PState{layout_ctx} -> s{layout_ctx = ctx : layout_ctx}

popLayoutContext :: Parser ()
popLayoutContext = do
  ctx <- gets layout_ctx
  case ctx of
    [] -> customFailure PopEmptyLayoutCtx
    (_ : ctxs) -> modify $ \s -> s{layout_ctx = ctxs}

-- parse :: Parser a -> String -> String -> Either String a
-- parse p fname source = do
--   lexemes <- lex fname source
--   let out =
--         runParserT p fname (TokStream lexemes source)
--           & flip evalState initPState
--   case out of
--     Left errBundle -> Left $ errorBundlePretty errBundle
--     Right val -> Right val

-- updatePos :: Parser ()
-- updatePos = do
--     ls <- lexemes <$> getInput
--     prevLine <- unPos . sourceColumn <$> getSourcePos
--     unless (null ls) $ do
--     let srcSpan = case ls of
--             (Located _ TokIndent : Located s _ : _) -> s
--             (Located s _ : _) -> s
--         nextLoc = srcSpanStart srcSpan
--         nextSrcPos = mkSrcPos nextLoc
--     updateParserState (\s@P.State{ statePosState } ->
--         let newStatePosState = statePosState{ pstateSourcePos = nextSrcPos }
--         in s{ statePosState = newStatePosState })

satisfy :: (Token -> Bool) -> Parser Lexeme
satisfy p = do
  D.dbg "analyze" $ do
    st <- getParserState
    return (stateOffset st, statePosState st)
  try $ guardIndentation >> P.satisfy (p . unLoc) <* setOrdGT
  where
    setOrdGT = modify $ \s -> s{indentOrd = GT}

guardIndentation :: Parser ()
guardIndentation = D.dbg "guardIndentation" $ do
  check <- optional $ P.satisfy (\t -> unLoc t == TokIndent)
  when (isJust check) $ do
    mlc <- currentLayoutContext
    case mlc of
      Nothing -> return ()
      Just Explicit -> return ()
      Just (Implicit indent) -> implicit indent
  where
    implicit :: Pos -> Parser ()
    implicit indent = do
      ord <- D.dbg "gets indentOrd" $ gets indentOrd
      D.dbg "current indent" L.indentLevel
      void $ L.indentGuard (return ()) ord indent

token :: Token -> Parser Lexeme
token t = satisfy (== t) <?> pretty t & show

---------------------------------------------------------------------------------------
-- Helper Combinators
---------------------------------------------------------------------------------------

oneOf :: [Token] -> Parser Lexeme
oneOf ts = satisfy (`elem` ts)

noneOf :: [Token] -> Parser Lexeme
noneOf ts = satisfy (not . (`elem` ts))

anyToken :: Parser Lexeme
anyToken = satisfy (const True)

reserved :: String -> Parser Lexeme
reserved word = satisfy (== reservedIdToTok word)

reservedOp :: String -> Parser Lexeme
reservedOp op = satisfy (== reservedOpToTok op)

parens :: Parser a -> Parser a
parens = between (token TokLParen) (token TokRParen)

braces :: Parser a -> Parser a
braces = between (token TokLBrace) (token TokRBrace)

brackets :: Parser a -> Parser a
brackets = between (token TokLBracket) (token TokRBracket)

backticks :: Parser a -> Parser a
backticks = between (token TokBackquote) (token TokBackquote)

comma :: Parser ()
comma = void $ token TokComma

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p comma

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p comma

semicolon :: Parser ()
semicolon = void $ token TokSemicolon

varid :: Parser ParsedName
varid = do
  Located loc tok <- satisfy isVarIdToken
  return $ case tok of
    TokVarId name -> mkUnQual varName loc name
    TokQualVarId qual name -> mkQual varName loc (qual, name)

varsym :: Parser ParsedName
varsym = do
  Located loc tok <- satisfy isVarSymToken
  return $ case tok of
    TokVarSym name -> mkUnQual varName loc name
    TokQualVarSym qual name -> mkQual varName loc (qual, name)

tyconid :: Parser ParsedName
tyconid = do
  Located loc tok <- satisfy isConIdToken
  return $ case tok of
    TokConId name -> mkUnQual tcName loc name
    TokQualConId qual name -> mkQual tcName loc (qual, name)

tyclsid :: Parser ParsedName
tyclsid = tyconid

dataconid :: Parser ParsedName
dataconid = do
  Located loc tok <- satisfy isConIdToken
  return $ case tok of
    TokConId name -> mkUnQual dataName loc name
    TokQualConId qual name -> mkQual dataName loc (qual, name)

dataconsym :: Parser ParsedName
dataconsym = do
  Located loc tok <- satisfy isConSymToken
  return $ case tok of
    TokConSym name -> mkUnQual dataName loc name
    TokQualConSym qual name -> mkQual dataName loc (qual, name)

tyvarid :: Parser ParsedName
tyvarid = do
  Located loc tok <- satisfy isVarIdToken
  return $ case tok of
    TokVarId name -> mkUnQual tvName loc name

modlName :: Parser Text
modlName = do
  Located _ tok <- satisfy isConIdToken
  return $ case tok of
    TokConId name -> name
    TokQualConId p1 p2 -> p1 <> "." <> p2

block :: Parser a -> Parser [a]
block p =
  (catMaybes <$>) $
    explicitBlock <|> implicitBlock
  where
    explicitBlock =
      between openExplicit closeExplicit $
        optional p `sepBy` semicolon

    implicitBlock =
      between openImplicit closeImplicit $
        P.many $
          align $
            Just <$> p

openExplicit = do
  token TokLBrace
  pushLayoutContext Explicit

closeExplicit = do
  token TokRBrace
  popLayoutContext

openImplicit = do
  current <- L.indentLevel
  pushLayoutContext $ Implicit current

closeImplicit = popLayoutContext

align :: Parser a -> Parser a
align = (>>) (modify $ \s -> s{indentOrd = EQ})

charLiteral = do
  (Located _ (TokLitChar t)) <- satisfy $
    \case
      TokLitChar _ -> True
      _ -> False
  let Right r = P.parse (L.charLiteral @Char) "" t
  pure r

expectedChar :: Char -> Parser Char
expectedChar c = do
  x@(Located _ (TokLitChar t)) <- satisfy $
    \case
      TokLitChar _ -> True
      _ -> False
  if Text.singleton c == t
    then pure c
    else failure (Just (Tokens $ NE.singleton x)) (Set.singleton $ Tokens $ NE.singleton x)

stringLiteral :: Parser Text
stringLiteral = do
  Located _ (TokLitString t) <-
    satisfy
      (\case TokLitString _ -> True; _ -> False)
  s <- do
    expectedChar '"'
    manyTill charLiteral (expectedChar '"')
  pure $ Text.pack s

integer :: Parser Integer
integer = do
  Located _ (TokLitInteger t) <-
    satisfy
      (\case TokLitInteger _ -> True; _ -> False)
  let Right i = P.parse (L.decimal @Char) "" t
  pure i
