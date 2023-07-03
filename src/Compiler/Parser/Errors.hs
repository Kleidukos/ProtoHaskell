module Compiler.Parser.Errors (prettyParseError) where

import Data.Char
import Prettyprinter
import Text.Parsec.Error
import Text.Parsec.Pos

import Compiler.BasicTypes.SrcLoc
import Compiler.Parser.Lexer
import Utils.Output

prettyParseError :: ParseError -> String -> [Lexeme] -> Doc ann
prettyParseError pe "" [] = pretty $ show pe -- Use pretty . show instead of pretty in case
-- definition of pretty changes to use this function
prettyParseError pe "" _ = pretty $ show pe
prettyParseError pe _ [] = pretty $ show pe
prettyParseError pe src lexemes =
  let pos = errorPos pe
      msgs = errorMessages pe
      line = sourceLine pos
      col = sourceColumn pos
      info = findInfo line col src lexemes
      infoAmount = case info of
        (Nothing, Nothing) -> NoInfo
        (Just _, Nothing) -> Source
        (Just _, Just _) -> SourceAndLexeme
        (Nothing, Just _) -> Lexeme
      spaces = hcat $ replicate (length (show line) + 1) space
      template src arrows =
        (spaces <> pipe)
          $+$ (pretty (show line) <+> pipe <+> src)
          $+$ (spaces <> pipe <+> arrows)
   in mkErrorMessage infoAmount info pos msgs template

data InfoAmount = NoInfo | Source | SourceAndLexeme | Lexeme

findInfo :: Line -> Column -> String -> [Lexeme] -> (Maybe String, Maybe Lexeme)
findInfo line col src lexs =
  (searchString src line col, searchLexemes lexs line col)

searchString :: String -> Line -> Column -> Maybe String
searchString src line col = do
  let srcLines = lines src
  if length srcLines < line
    then Nothing
    else Just $ srcLines !! (line - 1)

searchLexemes :: [Lexeme] -> Line -> Column -> Maybe Lexeme
searchLexemes lexs line col =
  let assocList =
        map
          ( \l@(Located span _) ->
              let start = srcSpanStart span
               in ((unsafeLocLine start, unsafeLocCol start), l)
          )
          . filter (\(Located _ t) -> t /= TokIndent)
          $ lexs
   in lookup (line, col) assocList

showPos :: SourcePos -> String
showPos p = show (sourceLine p) ++ ":" ++ show (sourceColumn p)

mkErrorMessage
  :: InfoAmount -- How detailed can our source/arrows be
  -> (Maybe String, Maybe Lexeme) -- Components for source/arrows
  -> SourcePos -- Position of error
  -> [Message] -- Parsec error messages
  -> (Doc ann -> Doc ann -> Doc ann) -- Callback to template
  -- source/arrows into msg
  -> Doc ann
mkErrorMessage infoAmt info pos msgs template =
  let prettySource = case infoAmt of
        NoInfo -> mempty
        Source -> templateSource
        SourceAndLexeme -> templateSourceAndLexeme
        Lexeme -> templateLexeme
   in header pos $+$ prettySource $+$ errMsgBody msgs
  where
    getBodyAndArrowWs src =
      let col = sourceColumn pos
          (leadingWS, body) = span isSpace src
          initSrcLoc = mkRealSrcLoc "" (sourceLine pos) 1
          bodyStartLoc = foldl advanceSrcLoc initSrcLoc leadingWS
          arrowWs = replicate (col - realSrcLocCol bodyStartLoc) space
       in (pretty body, hcat arrowWs)

    templateSource =
      let (Just src, _) = info
          (body, arrowWs) = getBodyAndArrowWs src
          arrows = pretty '^'
       in template body (arrowWs <> arrows)

    templateSourceAndLexeme =
      let (Just src, Just (Located (RealSrcSpan span) _)) = info
          (body, arrowWs) = getBodyAndArrowWs src
          mNumArrows = realSrcSpanLength span
       in case mNumArrows of
            Nothing -> "<Can't display source>"
            Just num -> template body (arrowWs <> hcat (replicate num $ pretty '^'))

    templateLexeme =
      let (_, Just (Located _ token)) = info
          body = pretty token
          arrows = hcat $ replicate (length (show token)) $ pretty '^'
       in case length (lines (show token)) of
            1 -> template body arrows
            _ -> "<Can't display source>"

header :: SourcePos -> Doc ann
header pos = "Parse error at" <+> pretty (showPos pos) <> ":"

errMsgBody :: [Message] -> Doc ann
errMsgBody =
  pretty
    . showErrorMessages
      "or"
      "unknown parse error"
      "expecting"
      "unexpected"
      "end of input"
