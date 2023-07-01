import Test.Tasty

import Compiler.Parser.ParserTest qualified as Parser
import Compiler.RenamerTest qualified as Renamer

main :: IO ()
main = defaultMain $ testGroup "ProtoHaskell Tests"
        [ Renamer.spec
        , Parser.tests
        ]
