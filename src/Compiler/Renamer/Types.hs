module Compiler.Renamer.Types where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)

import Compiler.BasicTypes.FastString
import Compiler.BasicTypes.Name
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhType
import Compiler.Settings
import Data.Vector (Vector)

data RenamerError
  = NoTopLevelSignature FastString
  | DuplicateBinding FastString (Vector SrcSpan)
  | DuplicateSignature FastString
  | BindingNotFound FastString
  deriving stock (Eq, Show)

data RenamerContext = RenamerContext
  { bindings :: Set Name
  , signatures :: Map Name (PhType Name)
  }
  deriving stock (Eq, Show)

emptyRenamerContext :: RenamerContext
emptyRenamerContext =
  RenamerContext
    { bindings = Set.empty
    , signatures = Map.empty
    }

data TopLevelBindings = TopLevelBindings
  { topLevelBindings :: Set Name
  , topLevelSignatures :: Map Name (PhType Name)
  }
  deriving stock (Eq, Ord, Show)

emptyTopLevelContext :: TopLevelBindings
emptyTopLevelContext =
  TopLevelBindings
    { topLevelBindings = Set.empty
    , topLevelSignatures = Map.empty
    }

type Renamer a =
  Eff
    [ Reader UniqueSupply
    , Reader RenamerContext
    , Reader Settings
    , State TopLevelBindings
    , Error RenamerError
    , IOE
    ]
    a
