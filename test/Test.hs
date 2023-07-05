module Test
  ( assertRight
  , assertRenamerError
  , pShowNoColorIndent2
  ) where

import Test.Tasty.HUnit

import Data.Text.Lazy qualified as TL

import Compiler.Renamer (RenamerError)
import Text.Pretty.Simple

pShowNoColorIndent2 :: (Show a) => a -> TL.Text
pShowNoColorIndent2 =
  pShowOpt
    defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 2
      , outputOptionsColorOptions = Nothing
      }

assertRight :: (Show a, HasCallStack) => Either a b -> IO b
assertRight (Left a) = do
  putStrLn "Test returned Left instead of Right"
  assertFailure $ "Got: " <> show a
assertRight (Right b) = pure b

assertRenamerError :: (Show a) => RenamerError -> Either RenamerError a -> IO ()
assertRenamerError expectedError (Left actualError)
  | actualError == expectedError = pure ()
  | otherwise = do
      putStrLn "Test returned an unexpected Renamer error"
      assertFailure $ "Expected: " <> show expectedError <> "\nGot: " <> show actualError
assertRenamerError _ (Right a) = do
  putStrLn "Test returned Right instead of Left"
  assertFailure $ "Got: " <> show a
