module Compiler.BaseEnvironment where

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.Unique (UniqueSupply)
import Compiler.Parser.Helpers
import Compiler.PhSyn.PhType
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader

getBaseEnvironment :: UniqueSupply -> IO (Set Name, Map Name (PhType Name))
getBaseEnvironment uniqueSupply =
  mkBaseEnvironment
    & Reader.runReader uniqueSupply
    & runEff

mkBaseEnvironment
  :: (Reader UniqueSupply :> es, IOE :> es)
  => Eff es (Set Name, Map Name (PhType Name))
mkBaseEnvironment = do
  baseFunctions <- mkBaseFunctions
  baseTypes <- mkBaseTypes
  pure (baseFunctions, baseTypes)

mkBaseFunctions :: (Reader UniqueSupply :> es, IOE :> es) => Eff es (Set Name)
mkBaseFunctions = do
  functions <-
    sequence
      [ mkAddName
      ]
  pure $ Set.fromList functions

mkBaseTypes :: (Reader UniqueSupply :> es, IOE :> es) => Eff es (Map Name (PhType Name))
mkBaseTypes = do
  addName <- mkAddName
  addType <- mkAddType
  let types =
        [ (addName, addType)
        ]
  pure $ Map.fromList types

mkAddName :: (Reader UniqueSupply :> es, IOE :> es) => Eff es Name
mkAddName = mkSystemName "+"

mkAddType :: (Reader UniqueSupply :> es, IOE :> es) => Eff es (PhType Name)
mkAddType = do
  intType <- PhVarTy <$> mkSystemTypeName "Int"
  pure $
    PhFunTy
      (Located noSrcSpan intType)
      ( Located noSrcSpan $
          PhFunTy
            (Located noSrcSpan intType)
            (Located noSrcSpan intType)
      )
