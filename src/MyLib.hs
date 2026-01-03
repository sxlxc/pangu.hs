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
  sym <- try (some (char ':')) <|> count 1 (char '.')
  _ <- many (char ' ')
  rcjk <- cjkChar
  let transformedsym = map convertToFullwidth sym
  return $ T.pack $ [lcjk] ++ transformedsym ++ [rcjk]

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
  _ <- char ':'
  an <- alphaNumChar
  return $ T.pack $ [cjk] ++ "：" ++ [an]

-- quotes
-- seems confusing ...
quotesym :: [Char]
quotesym = "'`\x05f4\""

cjkquote :: Rule
cjkquote = do
  cjk <- cjkChar
  quote <- oneOf quotesym
  return $ T.pack $ [cjk] ++ " " ++ [quote]

quoteCJK :: Rule
quoteCJK = do
  quote <- oneOf quotesym
  cjk <- cjkChar
  return $ T.pack $ [quote] ++ " " ++ [cjk]

fixQuote :: Rule
fixQuote = do
  openQuotes <- T.pack <$> some (oneOf quotesym)
  _ <- many spaceChar
  content <- T.pack <$> someTill anySingle (lookAhead $ some (oneOf quotesym))
  closeQuotes <- T.pack <$> some (oneOf quotesym)
  return $ openQuotes <> T.strip content <> closeQuotes

cjkpossessivequote :: Rule
cjkpossessivequote = do
  cjk <- cjkChar
  _ <- char '\''
  _ <- lookAhead $ anySingleBut 's'
  return $ T.pack $ cjk : " '"

-- This singlequoteCJK rule will turn '你好' into ' 你好'
-- which seems not desirable...
-- however, the behavior is aligned with python version
singlequoteCJK :: Rule
singlequoteCJK = do
  _ <- char '\''
  cjk <- cjkChar
  return $ T.pack $ "' " ++ [cjk]

fixPossessivequote :: Rule
fixPossessivequote = do
  pre <- cjkChar <|> alphaNumChar
  _ <- some spaceChar
  _ <- chunk "'s"
  return $ T.pack $ pre : "'s"

-- hash
-- hashANSCJKhash :: Rule
-- hashANSCJKhash = do
--   cjk1 <- cjkChar
--   _ <- char '#'


-- rule set, the order matters
myRules :: RuleSet
myRules =
  [ fullwidthCJKsymCJK,
    fullwidthCJKsym,
    dotsCJK,
    fixCJKcolAN,
    cjkquote,
    quoteCJK,
    fixQuote,
    cjkpossessivequote,
    -- singlequoteCJK,
    fixPossessivequote,
    empty -- a dummy rule
  ]