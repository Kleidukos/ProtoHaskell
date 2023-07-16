module Test
  ( assertRight
  , assertRenamerError
  , assertRenamerRight
  ) where

import Test.Tasty.HUnit

import Data.Text.Lazy qualified as TL

import Compiler.Renamer (RenamerError)
import Compiler.Renamer.Utils (pShowNoColorIndent2)

assertRight
  :: (Show a)
  => Either a b
  -> IO b
assertRight (Left a) = do
  putStrLn "Test returned Left instead of Right"
  assertFailure $ "Got: " <> show a
assertRight (Right b) = pure b

assertRenamerRight
  :: (Show a)
  => Either a b
  -> IO b
assertRenamerRight (Left a) = do
  putStrLn "Test returned Left instead of Right"
  assertFailure $ "Got: " <> show a
assertRenamerRight (Right b) = pure b

assertRenamerError
  :: (Show a)
  => RenamerError
  -> Either RenamerError a
  -> IO ()
assertRenamerError expectedError (Left actualError)
  | actualError == expectedError = pure ()
  | otherwise = do
      putStrLn "Test returned an unexpected Renamer error"
      assertFailure $ "Expected: " <> show expectedError <> "\nGot: " <> TL.unpack (pShowNoColorIndent2 actualError)
assertRenamerError _ (Right a) = do
  putStrLn "Test returned Right instead of Left"
  assertFailure $ "Got: " <> TL.unpack (pShowNoColorIndent2 a)
