{-# LANGUAGE DataKinds #-}

module Compiler.BasicTypes.Unique where

import Effectful
import Effectful.State.Static.Local
import Utils.Outputable (Outputable (ppr), text)

data Unique = Unique !UniqueSection !Int
  deriving (Eq, Ord, Show)

-- | Provenance of Uniques.
data UniqueSection
  = -- PHC passes
    ParseSection
  | RenameSection
  | TypeCheckSection
  | DesugarSection
  | SimplifySection
  | FastStringSection
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype UniqueSupply = UniqueSupply {uniques :: [Unique]}
  deriving newtype (Eq, Show)

mkUniqueSupply :: UniqueSection -> UniqueSupply
mkUniqueSupply pass = UniqueSupply $ map (Unique pass) [0 ..]

-- nextUnique :: (State UniqueSupply :> es) => Eff es Unique
-- nextUnique = state (\(UniqueSupply s) -> (head s, UniqueSupply $ tail s))

class HasUnique a where
  getUnique :: a -> Unique

instance HasUnique Unique where
  getUnique = id

instance Outputable UniqueSection where
  ppr ParseSection = text "p"
  ppr RenameSection = text "rn"
  ppr TypeCheckSection = text "tc"
  ppr DesugarSection = text "ds"
  ppr SimplifySection = text "simpl"
  ppr FastStringSection = text "fs"

instance Outputable Unique where
  ppr (Unique pass num) = ppr pass <> ppr num
