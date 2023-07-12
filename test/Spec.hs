import Test.Tasty

import Compiler.ParserTest qualified as Parser
import Compiler.RenamerTest qualified as Renamer
import Compiler.TypeCheckerTest qualified as TypeChecker
import Test.Tasty.Focus (withFocus)

main :: IO ()
main =
  defaultMain $
    withFocus $
      testGroup
        "ProtoHaskell Tests"
        [ Parser.spec
        , Renamer.spec
        , TypeChecker.spec
        ]
