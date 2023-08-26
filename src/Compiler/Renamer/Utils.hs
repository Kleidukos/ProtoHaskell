module Compiler.Renamer.Utils where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State

import Compiler.Settings
import Control.Monad (when)
import Data.Function
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.PhSyn.PhType
import Compiler.Renamer.Types
import Prettyprinter
import Text.Pretty.Simple
import Utils.Output
import Debug.Trace

addBinding :: Name -> Renamer a -> Renamer a
addBinding name action = do
  traceRenamer $ "Adding binding " <> pShowNoColorIndent2 name
  env <- Reader.ask @RenamerContext
  TopLevelBindings{topLevelBindings} <- State.get
  if bindingMember name topLevelBindings || bindingMember name env.bindings
    then Error.throwError $ DuplicateBinding name.occ.nameFS (Vector.singleton name.occ.occNameSrcSpan)
    else Reader.local (const $ RenamerContext (Set.insert name env.bindings) env.signatures) action

addBindings :: Vector Name -> Renamer a -> Renamer a
addBindings names action = do
  traceRenamer $ "Adding bindings " <> (outputLazy . pretty $ names)
  env <- Reader.ask @RenamerContext
  TopLevelBindings{topLevelBindings} <- State.get
  if any (\n -> bindingMember n topLevelBindings) names
    || any (\n -> bindingMember n env.bindings) names
    then do
      let duplicateName = names & Vector.head & (.occ.nameFS)
      let duplicateSpans = Vector.empty

      Error.throwError $ DuplicateBinding duplicateName duplicateSpans
    else do
      let newBindings = Vector.foldr' Set.insert env.bindings names
      Reader.local (const $ RenamerContext newBindings env.signatures) action

addSignature :: Name -> PhType Name -> (Renamer a -> Renamer a)
addSignature sigName sigType action = do
  env <- Reader.ask @RenamerContext
  TopLevelBindings{topLevelSignatures} <- State.get
  if signatureMember sigName topLevelSignatures || signatureMember sigName env.signatures
    then Error.throwError $ DuplicateSignature sigName.occ.nameFS
    else Reader.local (const $ RenamerContext env.bindings (Map.insert sigName sigType env.signatures)) action

addTopLevelSignature :: Name -> PhType Name -> Renamer ()
addTopLevelSignature sigName sigType = do
  traceRenamer $ "Adding top-level signature: " <> (outputLazy . pretty $ sigName)
  traceRenamer $ "With type: " <> (outputLazy . pretty $ sigType)
  State.modifyM
    ( \env -> do
        if signatureMember sigName env.topLevelSignatures
          then do
            traceRenamer "Duplicate signature"
            Error.throwError $ DuplicateSignature sigName.occ.nameFS
          else do
            let newTopLevelSigs = Map.insert sigName sigType env.topLevelSignatures
            let result =
                  TopLevelBindings
                    env.topLevelBindings
                    newTopLevelSigs
            pure result
    )

addTopLevelBinding :: Name -> Renamer ()
addTopLevelBinding name = do
  traceRenamer $ "Adding top-level binding " <> (outputLazy . pretty $ name)
  State.modifyM
    ( \env -> do
        if bindingMember name env.topLevelBindings
          then Error.throwError $ DuplicateBinding name.occ.nameFS (Vector.singleton name.occ.occNameSrcSpan)
          else
            pure $
              TopLevelBindings
                (Set.insert name env.topLevelBindings)
                env.topLevelSignatures
    )

-- | Determine if a local signature has already been processed.
signatureMember :: Name -> Map Name (PhType Name) -> Bool
signatureMember name sigMap =
  let result =
        length $
          Map.filterWithKey
            (\sig _value -> sig.occ.nameFS == name.occ.nameFS)
            sigMap
   in case result of
        0 -> False
        _ -> True

bindingMember :: Name -> Set Name -> Bool
bindingMember name bindings =
  let result =
        length $
          Set.filter
            (\binding -> binding.occ.nameFS == name.occ.nameFS)
            bindings
   in case result of
        0 -> False
        _ -> True

printContext :: String -> Renamer ()
printContext tag = do
  context <- Reader.ask @RenamerContext
  topLevelSignatures <- State.get @TopLevelBindings
  liftIO $
    TL.putStrLn $
      pShowNoColorIndent2
        [("context" <> tag, context, "top_level_signatures" :: String, topLevelSignatures)]

pShowNoColorIndent2 :: (Show a) => a -> TL.Text
pShowNoColorIndent2 =
  pShowOpt
    defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 2
      , outputOptionsColorOptions = Nothing
      }

traceRenamer :: (Reader Settings :> es, IOE :> es) => TL.Text -> Eff es ()
traceRenamer msg = do
  settings <- Reader.ask
  when (gOpt TraceRenamer settings) $ do
    liftIO $ traceEventIO $ "[Renamer]" <> TL.unpack msg
    liftIO $
      TL.putStrLn $
        "[Renamer] " <> msg
