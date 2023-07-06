module Compiler.TypeCheckerTest where

import Data.Map.Strict qualified as Map
import Test.Tasty
import Test.Tasty.HUnit

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName hiding (varName)
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType
import Compiler.TypeChecker
import Test

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
        "Type checking"
        [ testCase "Var lookup" testLookupVar
        , testCase "Expression with type annotation" testAnnotatedExpr
        ]
    ]

testTypeSynthesisOnStringLiteral :: Assertion
testTypeSynthesisOnStringLiteral = do
  let stringLiteral = PhLit (LitString "Parser Golden Tests")
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
        (inferType stringLiteral)
  assertEqual
    "Correct type inferred for String"
    result
    expectedType

testTypeSynthesisOnCharLiteral :: Assertion
testTypeSynthesisOnCharLiteral = do
  let stringLiteral = PhLit (LitChar 'c')
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
        (inferType stringLiteral)
  assertEqual
    "Correct type inferred for Char"
    result
    expectedType

testTypeSynthesisOnIntLiteral :: Assertion
testTypeSynthesisOnIntLiteral = do
  let stringLiteral = PhLit (LitInt 3)
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
        (inferType stringLiteral)
  assertEqual
    "Correct type inferred for Int"
    result
    expectedType

testTypeSynthesisOnFloatLiteral :: Assertion
testTypeSynthesisOnFloatLiteral = do
  let stringLiteral = PhLit (LitFloat 3.2)
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
        (inferType stringLiteral)
  assertEqual
    "Correct type inferred for Float"
    result
    expectedType

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
    result
    expectedType

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
    result
    expectedType
