{-# LANGUAGE DataKinds #-}

module Compiler.BasicTypes.Unique where

import Control.Concurrent.Counter (Counter)
import Control.Concurrent.Counter qualified as Counter
import Effectful
import Effectful.Reader.Static
import Effectful.Reader.Static qualified as Reader
import Prettyprinter

data Unique = Unique !UniqueSection !Int
  deriving (Eq, Ord, Show)

-- | Provenance of Uniques.
data UniqueSection
  = -- PHC passes
    SystemUnique
  | ParseSection
  | RenameSection
  | TypeCheckSection
  | DesugarSection
  | SimplifySection
  | FastStringSection
  deriving (Eq, Ord, Show, Enum, Bounded)

data UniqueSupply = UniqueSupply
  { section :: !UniqueSection
  , counter :: !Counter
  }
  deriving stock (Eq)

mkUniqueSupply :: UniqueSection -> IO UniqueSupply
mkUniqueSupply section = do
  counter <- Counter.new 0
  pure $ UniqueSupply section counter

nextUnique :: (Reader UniqueSupply :> es, IOE :> es) => Eff es Unique
nextUnique = do
  (UniqueSupply section counter) <- Reader.ask
  newUniqueInt <- liftIO $ Counter.get counter
  liftIO $ Counter.set counter (newUniqueInt + 1)
  pure $ Unique section newUniqueInt

instance Pretty UniqueSection where
  pretty SystemUnique = "sys"
  pretty ParseSection = "p"
  pretty RenameSection = "rn"
  pretty TypeCheckSection = "tc"
  pretty DesugarSection = "ds"
  pretty SimplifySection = "simpl"
  pretty FastStringSection = "fs"

instance Pretty Unique where
  pretty (Unique pass num) = pretty pass <> pretty num
