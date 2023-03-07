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
  ) where

import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique

import Utils.Outputable

-- | A 'Name' identifies an entity.
-- It appears in the AST after the Renamer has turned all 'ParsedName's
-- into 'Name's. Two names are compared by their 'uniq' field.
data Name = Name
  { sort :: NameSort
  , occ :: !OccName
  , uniq :: !Unique -- N.B. this unique disambiguates OccNames with the same unique
  -- because all OccNames with the same text share a unique.
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

instance Outputable NameSort where
  ppr Internal = text "internal"
  -- ppr External (Module _pkg name) = text "external" <+> parens (text name)
  ppr System = text "system"

instance HasSrcSpan Name where
  srcSpanOf name = srcSpanOf $ name.occ

instance HasOccName Name where
  occNameOf name = name.occ

instance HasUnique Name where
  getUnique name = name.uniq

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

instance Outputable Name where
  ppr Name{occ = name} = ppr name
