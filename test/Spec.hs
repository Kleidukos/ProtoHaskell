import Test.Tasty

import Compiler.Parser.LexerTest qualified as Lexer
import Compiler.Parser.ParserTest qualified as Parser
import Compiler.RenamerTest qualified as Renamer

tests :: IO TestTree
tests = do
  lexerTests <- Lexer.tests
  parserTests <- Parser.tests

  return $
    testGroup "Tests" $
      (lexerTests <> parserTests)
      ++ [Renamer.spec]

main :: IO ()
main = tests >>= defaultMain
