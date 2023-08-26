{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Compiler.TypeCheckerTest where

import Data.Map.Strict qualified as Map
import Test.Tasty
import Test.Tasty.HUnit

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName hiding (varName)
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique
import Compiler.Parser.Parser (parse)
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType
import Compiler.Renamer
import Compiler.Settings (defaultSettings, setRenamerTracing)
import Compiler.TypeChecker
import PyF
import Test
import Text.Pretty.Simple

spec :: TestTree
spec =
  testGroup
    "Type checker"
    [ testGroup
        "Type synthesis for literals"
        [ testCase "String" testTypeSynthesisOnStringLiteral
        , testCase "Char" testTypeSynthesisOnCharLiteral
        , testCase "Int" testTypeSynthesisOnIntLiteral
        , testCase "Float" testTypeSynthesisOnFloatLiteral
        ]
    , testGroup
        "Basic type checking"
        [ testCase "Var lookup" testLookupVar
        , testCase "Expression with type annotation" testAnnotatedExpr
        ]
    , testGroup
        "Numeric operations"
        [ testCase "Addition" testAddition
        ]
    ]

testTypeSynthesisOnStringLiteral :: Assertion
testTypeSynthesisOnStringLiteral = do
  let literal = PhLit (LitString "Parser Golden Tests")
  let expectedType =
        PhVarTy $
          Name
            { sort = Internal
            , occ =
                OccName
                  { nameSpace = TcClsName
                  , occNameSrcSpan = UnhelpfulSpan ""
                  , nameFS = "String"
                  }
            , uniq = Unique TypeCheckSection 0
            }
  result <-
    assertRight
      =<< runTypeChecker
        emptyTypeCheckerEnvironment
        (inferType literal)
  assertEqual
    "Correct type inferred for String"
    (TypedExpr expectedType literal)
    result

testTypeSynthesisOnCharLiteral :: Assertion
testTypeSynthesisOnCharLiteral = do
  let literal = PhLit (LitChar 'c')
  let expectedType =
        PhVarTy $
          Name
            { sort = Internal
            , occ =
                OccName
                  { nameSpace = TcClsName
                  , occNameSrcSpan = UnhelpfulSpan ""
                  , nameFS = "Char"
                  }
            , uniq = Unique TypeCheckSection 0
            }
  result <-
    assertRight
      =<< runTypeChecker
        emptyTypeCheckerEnvironment
        (inferType literal)
  assertEqual
    "Correct type inferred for Char"
    (TypedExpr expectedType literal)
    result

testTypeSynthesisOnIntLiteral :: Assertion
testTypeSynthesisOnIntLiteral = do
  let literal = PhLit (LitInt 3)
  let expectedType =
        PhVarTy $
          Name
            { sort = Internal
            , occ =
                OccName
                  { nameSpace = TcClsName
                  , occNameSrcSpan = UnhelpfulSpan ""
                  , nameFS = "Int"
                  }
            , uniq = Unique TypeCheckSection 0
            }
  result <-
    assertRight
      =<< runTypeChecker
        emptyTypeCheckerEnvironment
        (inferType literal)
  assertEqual
    "Correct type inferred for Int"
    (TypedExpr expectedType literal)
    result

testTypeSynthesisOnFloatLiteral :: Assertion
testTypeSynthesisOnFloatLiteral = do
  let literal = PhLit (LitFloat 3.2)
  let expectedType =
        PhVarTy $
          Name
            { sort = Internal
            , occ =
                OccName
                  { nameSpace = TcClsName
                  , occNameSrcSpan = UnhelpfulSpan ""
                  , nameFS = "Float"
                  }
            , uniq = Unique TypeCheckSection 0
            }
  result <-
    assertRight
      =<< runTypeChecker
        emptyTypeCheckerEnvironment
        (inferType literal)
  assertEqual
    "Correct type inferred for Float"
    result
    (TypedExpr expectedType literal)

testLookupVar :: Assertion
testLookupVar = do
  let varName =
        Name
          { sort = Internal
          , occ =
              OccName
                { nameSpace = VarName
                , occNameSrcSpan = UnhelpfulSpan ""
                , nameFS = "myVar"
                }
          , uniq = Unique TypeCheckSection 0
          }
  let var = PhVar varName

  let expectedType =
        PhVarTy $
          Name
            { sort = Internal
            , occ =
                OccName
                  { nameSpace = TcClsName
                  , occNameSrcSpan = UnhelpfulSpan ""
                  , nameFS = "Float"
                  }
            , uniq = Unique TypeCheckSection 1
            }
  let environment = Environment $ Map.singleton varName expectedType
  result <-
    assertRight
      =<< runTypeChecker
        environment
        (inferType var)
  assertEqual
    "Correct type inferred for myVar :: Float"
    (TypedExpr expectedType var)
    result

testAnnotatedExpr :: Assertion
testAnnotatedExpr = do
  let varName =
        Name
          { sort = Internal
          , occ =
              OccName
                { nameSpace = VarName
                , occNameSrcSpan = UnhelpfulSpan ""
                , nameFS = "myOtherVar"
                }
          , uniq = Unique TypeCheckSection 0
          }

  let expectedType =
        PhVarTy $
          Name
            { sort = Internal
            , occ =
                OccName
                  { nameSpace = TcClsName
                  , occNameSrcSpan = UnhelpfulSpan ""
                  , nameFS = "Int"
                  }
            , uniq = Unique TypeCheckSection 1
            }

  let var = Typed (Located noSrcSpan expectedType) (Located noSrcSpan (PhVar varName))

  let environment = Environment $ Map.singleton varName expectedType
  result <-
    assertRight
      =<< runTypeChecker
        environment
        (inferType var)
  assertEqual
    "Correct type inferred for myOtherVar :: Int"
    (TypedExpr expectedType (PhVar varName))
    result

testAddition :: Assertion
testAddition = do
  let snippet1 =
        [str|
  module Snippet1 where

    bar = 3 + 2
  |]
  parsedSnippet1 <- assertRight $ parse "<snippet1>" defaultSettings snippet1
  renamed <- assertRight =<< rename (setRenamerTracing defaultSettings) parsedSnippet1
  print renamed
