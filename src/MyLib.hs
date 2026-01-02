{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Replace.Megaparsec (streamEdit)
import Text.Megaparsec
import Text.Megaparsec.Char

-------------------------------------------------------------------------------
type Parser = Parsec Void Text

type Rule = Parser Text

type RuleSet = [Rule]

applyRules :: RuleSet -> Text -> Text
applyRules [] input = input
applyRules rules input = streamEdit (choice rules) id input

-- TEST RULES
appleToOrange :: Rule
appleToOrange = "orange" <$ chunk "apple"

emailAtRule :: Rule
emailAtRule = do
  prefix <- some (alphaNumChar <|> oneOf ("._%+-" :: String))
  _ <- char '@'
  suffix <- some (alphaNumChar <|> oneOf (".-" :: String))
  return $ T.pack prefix <> "[at]" <> T.pack suffix

-------------------------------------------------------------------------------
-- rules for pangu

-- | Check if a character falls within the CJK ranges provided
isCJK :: Char -> Bool
isCJK c = any (\(start, end) -> c >= start && c <= end) cjkRanges
  where
    cjkRanges =
      [ ('\x2e80', '\x2eff'),
        ('\x2f00', '\x2fdf'),
        ('\x3040', '\x309f'),
        ('\x30a0', '\x30fa'),
        ('\x30fc', '\x30ff'),
        ('\x3100', '\x312f'),
        ('\x3200', '\x32ff'),
        ('\x3400', '\x4dbf'),
        ('\x4e00', '\x9fff'),
        ('\xf900', '\xfaff')
      ]

convertToFullwidth :: Char -> Char
convertToFullwidth c = case c of
  ':' -> '：'
  '.' -> '。'
  '~' -> '～'
  '!' -> '！'
  '?' -> '？'
  ',' -> '，'
  ';' -> '；'
  _ -> c

-- A parser that matches a single CJK character
cjkChar :: Parser Char
cjkChar = satisfy isCJK

fullWidthSymbolRule :: Rule
fullWidthSymbolRule = do
  c1 <- cjkChar -- First CJK
  mid <-
    some $
      choice -- The "middle" symbol part
        [ char ' ',
          char ':',
          char '.'
        ]
  c2 <- cjkChar -- Second CJK

  -- In Haskell, we can actually process the 'mid' string logic here.
  -- For now, let's assume we want to turn ":" into "：" and "." into "。"
  let transformedMid = T.pack $ map convertToFullwidth mid
  return $ T.singleton c1 <> transformedMid <> T.singleton c2


-- the rule set
myRules :: RuleSet
myRules = [appleToOrange, emailAtRule, try fullWidthSymbolRule]