module Compiler.Parser.Pattern (parse) where

import Compiler.BasicTypes.Location
import Compiler.Parser.Helpers
import Compiler.PhSyn.PhExpr

parse :: Parser (Pat ParsedName)
parse = withNodeID $ \nodeID ->
  parseVar nodeID
    <|> parseWithinParentheses nodeID
    <|> literalPattern nodeID
    <|> token TokUnderscore $> PWildCard nodeID
    <|> (PCon nodeID <$> dataconid <*> many parse)
    <?> "pattern"

parseVar :: NodeID -> Parser (Pat ParsedName)
parseVar nodeID = PVar nodeID <$> varid

parseWithinParentheses :: NodeID -> Parser (Pat ParsedName)
parseWithinParentheses nodeID = do
  inside <- parens $ many parse
  pure $ case inside of
    [pat] -> ParPat nodeID pat
    pats -> PTuple nodeID pats

literalPattern :: NodeID -> Parser (Pat ParsedName)
literalPattern nodeID =
  (PLit nodeID <$>) $
    LitInt <$> integer
      <|> LitFloat <$> float
      <|> LitChar <$> charLiteral
      <|> LitString <$> stringLiteral
