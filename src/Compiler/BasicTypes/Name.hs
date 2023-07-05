{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Compiler.BasicTypes.Name
  ( Name (..)
  , NameSort (..)
  , nameSrcLoc
  , nameSrcSpan
  , isTyVarName
  , isTyConName
  , isDataConName
  , isValName
  , isVarName
  , isSystemName
  , mkTypeName
  , mkTermName
  ) where

import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique
import Data.Text
import Effectful
import Effectful.Reader.Static (Reader)
import Prettyprinter

-- | A 'Name' identifies an entity.
-- It appears in the AST after the Renamer has turned all 'ParsedName's
-- into 'Name's. Two names are compared by their 'uniq' field.
data Name = Name
  { sort :: NameSort
  , occ :: !OccName
  , uniq :: !Unique -- N.B. this unique disambiguates OccNames with the same unique
  }
  deriving stock (Show)

instance Eq Name where
  (==) n1 n2 = n1.uniq == n2.uniq

instance Ord Name where
  compare n1 n2 = compare n1.uniq n2.uniq

data NameSort
  = -- | A user-defined identifier defined in the module being compiled.
    Internal
  | -- | System-defined identifier.
    System
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty NameSort where
  pretty Internal = "internal"
  -- pretty External (Module _pkg name) =  "external" <+> parens (text name)
  pretty System = "system"

instance HasSrcSpan Name where
  srcSpanOf name = srcSpanOf $ name.occ

instance HasOccName Name where
  occNameOf name = name.occ

nameSrcLoc :: Name -> SrcLoc
nameSrcLoc name = srcSpanStart name.occ.occNameSrcSpan

nameSrcSpan :: Name -> SrcSpan
nameSrcSpan name = name.occ.occNameSrcSpan

isTyVarName :: Name -> Bool
isTyVarName name = isTyVarOccName $ name.occ

isTyConName :: Name -> Bool
isTyConName name = isTcClsOccName $ name.occ

isDataConName :: Name -> Bool
isDataConName name = isDataConOccName $ name.occ

isValName :: Name -> Bool
isValName name = isValOccName $ name.occ

isVarName :: Name -> Bool
isVarName name = isVarOccName $ name.occ

isSystemName :: Name -> Bool
isSystemName Name{sort = System} = True
isSystemName _ = False

mkTermName :: (Reader UniqueSupply :> es, IOE :> es) => Text -> Eff es Name
mkTermName fsName = do
  let occ = mkVarOccName (UnhelpfulSpan "") fsName
  let sort = Internal
  uniq <- nextUnique
  pure $ Name{..}

mkTypeName :: (Reader UniqueSupply :> es, IOE :> es) => Text -> Eff es Name
mkTypeName fsName = do
  let occ = mkTcOccName (UnhelpfulSpan "") fsName
  let sort = Internal
  uniq <- nextUnique
  pure $ Name{..}

instance Pretty Name where
  pretty Name{occ} = pretty occ
