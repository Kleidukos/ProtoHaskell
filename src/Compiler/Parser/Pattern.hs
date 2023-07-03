module Compiler.Parser.Pattern (parse, parseLocated) where

import Compiler.Parser.Helpers
import Compiler.PhSyn.PhExpr

parseLocated :: Parser (LPat ParsedName)
parseLocated = locate parse

parse :: Parser (Pat ParsedName)
parse =
  parseVar
    <|> parseWithinParentheses
    <|> literalPattern
    <|> token TokUnderscore $> PWildCard
    <|> (PCon <$> dataconid <*> many parse)
    <?> "pattern"

parseVar :: Parser (Pat ParsedName)
parseVar = PVar <$> varid

parseWithinParentheses :: Parser (Pat ParsedName)
parseWithinParentheses = do
  inside <- parens $ many parse
  pure $ case inside of
    [pat] -> ParPat pat
    pats -> PTuple pats

literalPattern :: Parser (Pat ParsedName)
literalPattern =
  (PLit <$>) $
    LitInt <$> integer
      <|> LitFloat <$> float
      <|> LitChar <$> charLiteral
      <|> LitString <$> stringLiteral
