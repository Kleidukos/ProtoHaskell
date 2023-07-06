{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Compiler.RenamerTest where

-- import Data.Text.Lazy.IO qualified as TL
import PyF
import Test.Tasty
import Test.Tasty.HUnit

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.SrcLoc
import Compiler.Parser.Parser (parse)
import Compiler.Renamer
import Compiler.Settings (defaultSettings)
import Control.Monad (void)
import Test

spec :: TestTree
spec =
  testGroup
    "Renamer"
    [ testCase "Rename a ParsedName into a Name" testRenameParsedName
    , testCase "Top-level bindings must have a type signature" testEnsureTopLevelBindingsHaveASignature
    , testCase "Duplicate bindings are caught" testDuplicateBindingsAreCaught
    , testCase "Bindings with the same name in different branches" testBindingsWithSameNameInDifferentBranches
    ]

testRenameParsedName :: Assertion
testRenameParsedName = do
  let srcSpan = mkUnhelpfulSrcSpan "<test>"
  let parsedName = mkUnQual varName srcSpan "putStrLn"
  result <- assertRight =<< runRenamer (renameParsedName parsedName)
  assertEqual "Wrong sort" result.sort Internal
  assertEqual "Wrong occurence name" result.occ (mkVarOccName srcSpan "putStrLn")

testEnsureTopLevelBindingsHaveASignature :: Assertion
testEnsureTopLevelBindingsHaveASignature = do
  let snippet1 =
        [str|
  module Snippet1 where
    
    foo = 3
  |]
  parsedSnippet1 <- assertRight $ parse "<snippet1>" defaultSettings snippet1
  assertRenamerError (NoTopLevelSignature "foo") =<< rename parsedSnippet1

testDuplicateBindingsAreCaught :: Assertion
testDuplicateBindingsAreCaught = do
  let snippet1 =
        [str|
  module Snippet1 where
    
    bar :: Int
    bar = 
      let x = "lol"
          x = "mdr" 
       in 3
  |]
  parsedSnippet1 <- assertRight $ parse "<snippet1>" defaultSettings snippet1
  -- TL.putStrLn $ pShowNoColorIndent2 parsedSnippet1
  assertRenamerError (DuplicateBinding "x") =<< rename parsedSnippet1

testBindingsWithSameNameInDifferentBranches :: Assertion
testBindingsWithSameNameInDifferentBranches = do
  let snippet1 =
        [str|
  module Snippet1 where
    
    bar :: String
    bar = 
      let x = "lol"
       in x

    otherFun :: String
    otherFun =
      let x = "blah"
       in x
  |]
  parsedSnippet1 <- assertRight $ parse "<snippet1>" defaultSettings snippet1
  void $ assertRight =<< rename parsedSnippet1
