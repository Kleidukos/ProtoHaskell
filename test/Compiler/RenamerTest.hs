{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Compiler.RenamerTest where

import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy.IO qualified as TL
import Data.Vector qualified as Vector
import Effectful.State.Static.Local qualified as State
import PyF
import Test.Tasty
import Test.Tasty.Focus (focus)
import Test.Tasty.HUnit

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique
import Compiler.Parser.Parser (parse)
import Compiler.PhSyn.PhType
import Compiler.Renamer
import Compiler.Renamer.Types
import Compiler.Renamer.Utils
import Compiler.Settings (defaultSettings)
import Test

spec :: TestTree
spec =
  testGroup
    "Renamer"
    [ testCase "Rename a ParsedName into a Name" testRenameParsedName
    , testCase "Renaming environment utils work" testRenamingUtils
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

testRenamingUtils :: Assertion
testRenamingUtils = do
  let topLevelBinding1 =
        Name
          { sort = Internal
          , occ = mkTcOccName noSrcSpan "bar"
          , uniq = Unique RenameSection 0
          }
  Right result <- runRenamer $ do
    addTopLevelBinding topLevelBinding1
    State.get @TopLevelBindings
  assertEqual
    "Expected top-level binding"
    result.topLevelBindings
    (Set.singleton topLevelBinding1)

  let topLevelSignatureName1 =
        Name
          { sort = Internal
          , occ = mkVarOccName noSrcSpan "bar"
          , uniq = Unique RenameSection 0
          }
  let topLevelSignatureType1 =
        PhVarTy 0 $
          Name
            { sort = Internal
            , occ = mkTcOccName noSrcSpan "Int"
            , uniq = Unique RenameSection 1
            }

  intermediateResult <- runRenamer $ do
    addTopLevelSignature topLevelSignatureName1 topLevelSignatureType1
    State.get @TopLevelBindings

  (result' :: TopLevelBindings) <- assertRenamerRight intermediateResult

  assertEqual
    "Expected top-level signature"
    result'.topLevelSignatures
    (Map.singleton topLevelSignatureName1 topLevelSignatureType1)

testEnsureTopLevelBindingsHaveASignature :: Assertion
testEnsureTopLevelBindingsHaveASignature = do
  let snippet1 =
        [str|
  module Snippet1 where
    
    foo = 3
  |]
  parsedSnippet1 <- assertRight =<< parse "<snippet1>" defaultSettings snippet1
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
  parsedSnippet1 <- assertRight =<< parse "<snippet1>" defaultSettings snippet1
  assertRenamerError
    ( DuplicateBinding "x" $
        Vector.fromList
          [ 0
          , 1
          ]
    )
    =<< rename parsedSnippet1

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
  parsedSnippet1 <- assertRight =<< parse "<snippet1>" defaultSettings snippet1
  void $ assertRight =<< rename parsedSnippet1
