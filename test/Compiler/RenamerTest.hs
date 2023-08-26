{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Compiler.RenamerTest where

import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Effectful.State.Static.Local qualified as State
import PyF
import Test.Tasty
import Test.Tasty.HUnit

import Compiler.BaseEnvironment
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
  baseSupply <- mkUniqueSupply SystemUnique
  baseEnvironment <- getBaseEnvironment baseSupply
  result <- assertRight =<< runRenamer defaultSettings baseEnvironment (renameParsedName parsedName)
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
  baseSupply <- mkUniqueSupply SystemUnique
  baseEnvironment <- getBaseEnvironment baseSupply
  Right result <- runRenamer defaultSettings baseEnvironment $ do
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
        PhVarTy $
          Name
            { sort = Internal
            , occ = mkTcOccName noSrcSpan "Int"
            , uniq = Unique RenameSection 1
            }

  intermediateResult <- runRenamer defaultSettings baseEnvironment $ do
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
  parsedSnippet1 <- assertRight $ parse "<snippet1>" defaultSettings snippet1
  baseSupply <- mkUniqueSupply SystemUnique
  baseEnvironment <- getBaseEnvironment baseSupply
  assertRenamerError (NoTopLevelSignature "foo") =<< rename defaultSettings baseEnvironment parsedSnippet1

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
  baseSupply <- mkUniqueSupply SystemUnique
  baseEnvironment <- getBaseEnvironment baseSupply
  assertRenamerError
    ( DuplicateBinding "x" $
        Vector.fromList
          [ RealSrcSpan $ mkRealSrcSpan (mkRealSrcLoc "<snippet1>" 6 11) (mkRealSrcLoc "<snippet1>" 6 20)
          , RealSrcSpan $ mkRealSrcSpan (mkRealSrcLoc "<snippet1>" 7 11) (mkRealSrcLoc "<snippet1>" 7 20)
          ]
    )
    =<< rename defaultSettings baseEnvironment parsedSnippet1

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
  baseSupply <- mkUniqueSupply SystemUnique
  baseEnvironment <- getBaseEnvironment baseSupply
  void $ assertRight =<< rename defaultSettings baseEnvironment parsedSnippet1
