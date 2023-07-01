{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
module Compiler.RenamerTest where

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.SrcLoc
import Compiler.Parser.Parser (parse)
import Compiler.Renamer
import Compiler.Settings (defaultSettings)
import PyF
import Test
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec = testGroup "Renamer"
  [ testCase "Rename a ParsedName into a Name" testRenameParsedName
  -- , testCase "Top-level bindings must have a type signature" testEnsureTopLevelBindingsHaveASignature
  , testCase "Duplicate bindings are caught" testDuplicateBindingsAreCaught
  ]

testRenameParsedName :: Assertion
testRenameParsedName =  do
  let srcSpan = mkUnhelpfulSrcSpan "<test>"
  let parsedName = mkUnQual varName srcSpan "putStrLn"
  result <- assertRight =<< runRenamer (renameParsedName parsedName)
  assertEqual "Wrong sort" result.sort Internal
  assertEqual "Wrong occurence name" result.occ (mkVarOccName srcSpan "putStrLn")

-- testEnsureTopLevelBindingsHaveASignature :: Assertion
-- testEnsureTopLevelBindingsHaveASignature = do
--   let snippet1 = [str|
--   module Snippet1 where
--     
--     foo :: Int
--     foo = 3
--   |]
--   parsedSnippet1 <- assertRight $ parse "<snippet1>" defaultSettings snippet1
--   (_, context) <- assertRight =<< rename parsedSnippet1
--   let nameTypes = context.nameTypes
--   let fooSrcSpan = mkUnhelpfulSrcSpan "<test>"
--   let fooName = Name Internal (mkVarOccName fooSrcSpan "foo") (Unique RenameSection 0)
--   assertEqual "foo is listed in the context" (Map.lookup fooName nameTypes) Nothing

testDuplicateBindingsAreCaught :: Assertion
testDuplicateBindingsAreCaught = do
  let snippet1 = [str|
  module Snippet1 where
    
    foo :: Int
    foo = 3

    foo :: Char
    foo = 'c'
  |]
  parsedSnippet1 <- assertRight $ parse "<snippet1>" defaultSettings snippet1
  assertRenamerError (DuplicateBinding "foo") =<< rename parsedSnippet1
