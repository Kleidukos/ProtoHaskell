module Utils.Output where

import Data.Char (isSymbol)
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

output :: Doc ann -> Text
output doc = renderStrict (layoutPretty defaultLayoutOptions doc)

output' :: Doc ann -> Text
output' doc = renderStrict (layoutSmart (defaultLayoutOptions) doc)

prettyWhen :: Bool -> Doc ann -> Doc ann
prettyWhen condition doc =
  if condition
    then doc
    else mempty

prettyUnless :: Bool -> Doc ann -> Doc ann
prettyUnless condition doc = prettyWhen (not condition) doc

asPrefixVar :: Doc ann -> Doc ann
asPrefixVar d
  | isOperatorDoc d = parens d
  | otherwise = d

asInfixVar :: Doc ann -> Doc ann
asInfixVar d
  | isOperatorDoc d = d
  | otherwise = pretty '`' <> d <> pretty '`'

isOperatorDoc :: Doc ann -> Bool
isOperatorDoc d =
  let str = output d
   in not (Text.null str) && isSymbol (Text.head str)

dcolon :: Doc ann
dcolon = "::"

arrow :: Doc ann
arrow = "->"

($+$) :: Doc ann -> Doc ann -> Doc ann
($+$) x y = align (vsep [x, y])
