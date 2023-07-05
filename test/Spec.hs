import Test.Tasty

import Compiler.ParserTest qualified as Parser
import Compiler.RenamerTest qualified as Renamer

main :: IO ()
main =
  defaultMain $
    testGroup
      "ProtoHaskell Tests"
      [ Renamer.spec
      , Parser.tests
      ]
