{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import Data.Function (fix)
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

applyUntilFixed :: Rule -> Text -> Text
applyUntilFixed rule =
  fix
    ( \loop current ->
        let next = streamEdit rule id current
         in if next == current then next else loop next
    )

applyRules :: RuleSet -> Text -> Text
applyRules rules input = foldl (flip applyUntilFixed) input rules

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
convertToFullwidth c =
  case c of
    ':' -> '：'
    '.' -> '。'
    '~' -> '～'
    '!' -> '！'
    '?' -> '？'
    ',' -> '，'
    ';' -> '；'
    '\"' -> '”'
    '\'' -> '’'
    _ -> c

-- A parser that matches a single CJK character
cjkChar :: Parser Char
cjkChar = satisfy isCJK

-- use python.py as reference for these rules

fullwidthCJKsymCJK :: Rule
fullwidthCJKsymCJK = do
  lcjk <- cjkChar
  _ <- many (char ' ')
  sym <- fmap T.unpack (chunk ".") <|> some (oneOf (":" :: [Char]))
  _ <- many (char ' ')
  rcjk <- cjkChar

  let transformedsym = T.pack $ map convertToFullwidth sym
  return $ T.pack [lcjk] <> transformedsym <> T.pack [rcjk]

fullwidthCJKsym :: Rule
fullwidthCJKsym = do
  cjk <- cjkChar
  _ <- many (char ' ')
  sym <- some $ oneOf ("~!?,;" :: [Char])
  _ <- many (char ' ')
  let transformedsym = T.pack $ map convertToFullwidth sym
  return $ T.pack [cjk] <> transformedsym

dotsCJK :: Rule
dotsCJK = do
  dots <- chunk "..." <|> chunk "…"
  cjk <- cjkChar
  return $ dots <> T.pack (" " ++ [cjk])

fixCJKcolAN :: Rule
fixCJKcolAN = do
  cjk <- cjkChar
  _ <- chunk ":"
  an <- alphaNumChar
  return $ T.pack $ [cjk] ++ "：" ++ [an]

cjkquote :: Rule
cjkquote = do
  cjk <- cjkChar
  quote <- oneOf ("\x05f4\"\'" :: [Char])
  return $ T.pack $ [cjk] ++ " " ++ [quote]

quoteCJK :: Rule
quoteCJK = do
  quote <- oneOf ("\x05f4\"\'" :: [Char])
  cjk <- cjkChar
  return $ T.pack $ [quote] ++ " " ++ [cjk]

-- the rule set
myRules :: RuleSet
myRules =
  [ fullwidthCJKsymCJK,
    fullwidthCJKsym,
    dotsCJK,
    fixCJKcolAN,
    cjkquote,
    quoteCJK
  ]