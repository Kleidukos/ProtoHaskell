import Test.Tasty

import Compiler.ParserTest qualified as Parser
import Compiler.RenamerTest qualified as Renamer
import Compiler.TypeCheckerTest qualified as TypeChecker

main :: IO ()
main =
  defaultMain $
    testGroup
      "ProtoHaskell Tests"
      [ Parser.spec
      , Renamer.spec
      , TypeChecker.spec
      ]
