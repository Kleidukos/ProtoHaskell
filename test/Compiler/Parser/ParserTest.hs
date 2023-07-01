module Compiler.Parser.ParserTest (tests) where

import Prelude hiding (lex)

import Test.Tasty
import Test.Tasty.Golden (writeBinaryFile, goldenVsFileDiff)
import Test
import Compiler.Parser.Parser (parse)
import Compiler.Parser.Helpers (defaultSettings)

tests :: TestTree
tests = testGroup 
        "Parser Golden Tests"
        [ shouldSucceed ]

successfulTests :: FilePath
successfulTests = "test/Compiler/Parser/testcases/shouldsucceed/"

shouldSucceed :: TestTree
shouldSucceed = testGroup "Should Succeed"
  [ goldenVsFileDiff
    "Basic"
    (\ref new -> ["diff", "-u", ref, new])
    (successfulTests <> "basic.expected")
    (successfulTests <> "basic.actual")
    testBasic
  ]

testBasic :: IO ()
testBasic = do
  let sourceFile = successfulTests <> "basic.hs"
  contents <- readFile sourceFile
  result <- assertRight $ parse sourceFile defaultSettings contents
  print result
  writeBinaryFile (successfulTests <> "basic.actual") (show result)

  
