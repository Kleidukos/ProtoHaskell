module Compiler.Parser.ParserTest (tests) where

import Prelude hiding (lex)

import Test.Tasty
import Test.Tasty.Golden (goldenVsFileDiff)
import Test
import Data.Text.Lazy.IO qualified as TL


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
  , goldenVsFileDiff
    "Geometry"
    (\ref new -> ["diff", "-u", ref, new])
    (successfulTests <> "geometry.expected")
    (successfulTests <> "geometry.actual")
    testGeometry
  , goldenVsFileDiff
    "Local"
    (\ref new -> ["diff", "-u", ref, new])
    (successfulTests <> "local.expected")
    (successfulTests <> "local.actual")
    testLocal
  , goldenVsFileDiff
    "Operators"
    (\ref new -> ["diff", "-u", ref, new])
    (successfulTests <> "operators.expected")
    (successfulTests <> "operators.actual")
    testOperators
  ]

testBasic :: IO ()
testBasic = do
  let sourceFile = successfulTests <> "basic.hs"
  contents <- readFile sourceFile
  result <- assertRight $ parse sourceFile defaultSettings contents
  TL.writeFile (successfulTests <> "basic.actual") (pShowNoColorIndent2 result)

testGeometry :: IO ()
testGeometry = do
  let sourceFile = successfulTests <> "geometry.hs"
  contents <- readFile sourceFile
  result <- assertRight $ parse sourceFile defaultSettings contents
  TL.writeFile (successfulTests <> "geometry.actual") (pShowNoColorIndent2 result)

testLocal :: IO ()
testLocal = do
  let sourceFile = successfulTests <> "local.hs"
  contents <- readFile sourceFile
  result <- assertRight $ parse sourceFile defaultSettings contents
  TL.writeFile (successfulTests <> "local.actual") (pShowNoColorIndent2 result)

testOperators :: IO ()
testOperators = do
  let sourceFile = successfulTests <> "operators.hs"
  contents <- readFile sourceFile
  result <- assertRight $ parse sourceFile defaultSettings contents
  TL.writeFile (successfulTests <> "operators.actual") (pShowNoColorIndent2 result)
