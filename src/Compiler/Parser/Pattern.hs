module Compiler.Parser.Pattern (parse, parseLocated) where

import Compiler.Parser.Helpers

parseLocated :: Parser (Located ParsedName)
parseLocated = locate parse

parse :: Parser ParsedName
parse = varid
