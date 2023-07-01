{-# LANGUAGE DataKinds #-}

module Compiler.BasicTypes.ParsedName
  ( ParsedName
  , mkUnQual
  , mkQual
  , mkOrig
  , parsedNameOcc
  , parsedNameSpace
  , getParsedName
  , isSrcParsedName
  ) where

import Data.Text (Text)

import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique

import GHC.Records
import Prettyprinter

-- | A 'ParsedName' is an identifier produced by the Parser.
data ParsedName
  = -- | An unqualified name directly from the source.
    UnQual OccName
  | -- | A qualified name directly from the source.
    -- the module name is the (possibly qualified) name of the module from which
    -- it is imported; not necessarily the module in which it is defined.
    Qual Text OccName
  | -- | 'Original' name. Module name is the defining module.
    -- We use these when we generate code and want to force the use
    -- of a specific function, eg, 'Prelude.map'.
    Orig Text OccName
  | -- | An exact 'Name'. Used when parsing syntax like '[]'.
    -- Can only be created by 'getParsedName'.
    Exact Name
  deriving (Eq, Ord, Show)

instance HasField "occ" ParsedName OccName where
  getField = \case
    (UnQual n) -> n
    (Qual _ n) -> n
    (Orig _ n) -> n
    (Exact n) -> n.occ

mkUnQual :: NameSpace -> SrcSpan -> Text -> ParsedName
mkUnQual ns ss t = UnQual (mkOccName ns ss t)

mkQual :: NameSpace -> SrcSpan -> (Text, Text) -> ParsedName
mkQual ns ss (qual, t) = Qual qual (mkOccName ns ss t)

mkOrig :: Text -> OccName -> ParsedName
mkOrig = Orig

instance HasOccName ParsedName where
  occNameOf = parsedNameOcc

instance HasSrcSpan ParsedName where
  srcSpanOf = srcSpanOf . occNameOf

instance HasUnique ParsedName where
  getUnique = getUnique . occNameOf

parsedNameOcc :: ParsedName -> OccName
parsedNameOcc (UnQual n) = n
parsedNameOcc (Qual _ n) = n
parsedNameOcc (Orig _ n) = n
parsedNameOcc (Exact n) = n.occ

parsedNameSpace :: ParsedName -> NameSpace
parsedNameSpace pn = pn.occ.nameSpace

getParsedName :: Name -> ParsedName
getParsedName = Exact

isSrcParsedName :: ParsedName -> Bool
isSrcParsedName (UnQual _) = True
isSrcParsedName (Qual _ _) = True
isSrcParsedName _ = False

instance Pretty ParsedName where
  pretty (UnQual n) = pretty n
  pretty (Qual qual n) = pretty qual <> dot <> pretty n
  pretty (Orig qual n) = pretty qual <> dot <> pretty n
  pretty (Exact name) = pretty name
