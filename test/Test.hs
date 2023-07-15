module Test
  ( assertRight
  , assertRenamerError
  , assertRenamerRight
  ) where

import Test.Tasty.HUnit

import Data.Text.Lazy qualified as TL

import Compiler.Renamer (RenamerError)
import Compiler.Renamer.Utils (pShowNoColorIndent2)
import Effectful.Error.Static

assertRight
  :: (Show a, HasCallStack)
  => Either a b
  -> IO b
assertRight (Left a) = do
  putStrLn "Test returned Left instead of Right"
  assertFailure $ "Got: " <> show a
assertRight (Right b) = pure b

assertRenamerRight
  :: (Show a, HasCallStack)
  => Either (CallStack, a) b
  -> IO b
assertRenamerRight (Left (callstack, a)) = do
  putStrLn "Test returned Left instead of Right"
  putStrLn $ prettyCallStack callstack
  assertFailure $ "Got: " <> show a
assertRenamerRight (Right b) = pure b

assertRenamerError
  :: (Show a)
  => RenamerError
  -> Either (CallStack, RenamerError) a
  -> IO ()
assertRenamerError expectedError (Left (callstack, actualError))
  | actualError == expectedError = pure ()
  | otherwise = do
      putStrLn "Test returned an unexpected Renamer error"
      putStrLn $ prettyCallStack callstack
      assertFailure $ "Expected: " <> show expectedError <> "\nGot: " <> TL.unpack (pShowNoColorIndent2 actualError)
assertRenamerError _ (Right a) = do
  putStrLn "Test returned Right instead of Left"
  assertFailure $ "Got: " <> TL.unpack (pShowNoColorIndent2 a)
