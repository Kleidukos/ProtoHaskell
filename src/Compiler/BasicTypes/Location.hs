{-# LANGUAGE StrictData #-}

module Compiler.BasicTypes.Location where

import Data.Word

newtype NodeId = NodeId
  { getNodeId :: Word32
  }
  deriving stock (Eq, Show, Ord)

data Span = Span
  { beginPos :: {-# UNPACK #-} Int
  , endPos :: {-# UNPACK #-} Int
  }
  deriving stock (Eq, Show, Ord)

data SpanMap = SpanMap
  { path :: FilePath
  }
