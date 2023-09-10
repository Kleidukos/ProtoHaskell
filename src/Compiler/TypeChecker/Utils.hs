module Compiler.TypeChecker.Utils where

import Compiler.Parser.Helpers
import Utils.Output

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Debug.Trace (traceEventIO)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Prettyprinter

traceRenamer :: (Reader Settings :> es, IOE :> es, Pretty a) => a -> Eff es ()
traceRenamer a = do
  settings <- Reader.ask
  when (gOpt TraceTypechecker settings) $ do
    let msg = outputLazy . pretty $ a
    liftIO $ traceEventIO $ "[Typechecker]" <> TL.unpack msg
    liftIO $
      TL.putStrLn $
        "[Typechecker] " <> msg
