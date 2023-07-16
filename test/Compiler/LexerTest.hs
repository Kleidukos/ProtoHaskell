module Compiler.LexerTest (spec) where

import Prelude hiding (lex)

import Data.Text.Lazy.IO qualified as TL
import Test
import Test.Tasty
import Test.Tasty.Golden (goldenVsFileDiff)

import Compiler.Parser.Lexer
import Compiler.Renamer.Utils (pShowNoColorIndent2)

spec :: TestTree
spec =
  testGroup
    "Lexer Golden Tests"
    [shouldSucceed]

successfulTests :: FilePath
successfulTests = "test/fixtures/Lexer/testcases/"

shouldSucceed :: TestTree
shouldSucceed =
  testGroup
    "Should Succeed"
    [ goldenVsFileDiff
        "Nested Comment"
        (\ref new -> ["diff", "-u", ref, new])
        (successfulTests <> "nestedComment.expected")
        (successfulTests <> "nestedComment.actual")
        nestedComment
    , goldenVsFileDiff
        "One Token"
        (\ref new -> ["diff", "-u", ref, new])
        (successfulTests <> "oneToken.expected")
        (successfulTests <> "oneToken.actual")
        testoneToken
    , goldenVsFileDiff
        "Simple File"
        (\ref new -> ["diff", "-u", ref, new])
        (successfulTests <> "simpleFile.expected")
        (successfulTests <> "simpleFile.actual")
        testsimpleFile
    ]

nestedComment :: IO ()
nestedComment = do
  let sourceFile = successfulTests <> "nestedComment.hs"
  contents <- readFile sourceFile
  result <- assertRight $ lex sourceFile contents
  TL.writeFile (successfulTests <> "nestedComment.actual") (pShowNoColorIndent2 result)

testoneToken :: IO ()
testoneToken = do
  let sourceFile = successfulTests <> "oneToken.hs"
  contents <- readFile sourceFile
  result <- assertRight $ lex sourceFile contents
  TL.writeFile (successfulTests <> "oneToken.actual") (pShowNoColorIndent2 result)

testsimpleFile :: IO ()
testsimpleFile = do
  let sourceFile = successfulTests <> "simpleFile.hs"
  contents <- readFile sourceFile
  result <- assertRight $ lex sourceFile contents
  TL.writeFile (successfulTests <> "simpleFile.actual") (pShowNoColorIndent2 result)
