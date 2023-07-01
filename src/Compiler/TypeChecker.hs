module Compiler.TypeChecker where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhType
import Effectful
import Effectful.Reader.Static

type Environment = Map Unique (LPhType Unique)

data TypeCheckingError = TCError

var
  :: (Reader Environment :> es)
  => Unique
  -> Eff es (Maybe (LPhType Unique))
var x = asks (Map.lookup x)

-- synthLiteral :: forall id. PhLit -> Unique
-- synthLiteral (LitInt i)     = Unique TypeCheckSection "Int"
-- synthLiteral (LitFloat f)   = Unique TypeCheckSection "Float"
-- synthLiteral (LitChar c)    = Unique TypeCheckSection "Char"
-- synthLiteral (LitString s)  = Unique TypeCheckSection "String"
