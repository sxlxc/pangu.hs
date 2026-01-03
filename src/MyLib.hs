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
        let next = streamEdit (try rule) id current
         in if next == current then next else loop next
    )

applyRulesRecursively :: RuleSet -> Text -> Text
applyRulesRecursively rules input = foldl (flip applyUntilFixed) input rules

applyRules :: RuleSet -> Text -> Text
applyRules rules input = foldl (flip applyOnce) input rules
  where
    applyOnce rule = streamEdit (try rule) id

-------------------------------------------------------------------------------
-- rules for pangu

-- alphaNumChar from megaparsec matches CJK chars...
-- need to implement a new one
alphanumericChar :: Parser Char
alphanumericChar = satisfy $ \c ->
  (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')

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
  an <- alphanumericChar
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
  pre <- cjkChar <|> alphanumericChar
  _ <- some spaceChar
  _ <- chunk "'s"
  return $ T.pack $ pre : "'s"

-- hash
hashANSCJKhash :: Rule
hashANSCJKhash = do
  cjk1 <- cjkChar
  _ <- char '#'
  mid <- some cjkChar
  _ <- char '#'
  cjk2 <- cjkChar
  return $ T.pack $ [cjk1] ++ " #" ++ mid ++ "# " ++ [cjk2]

cjkhash :: Rule
cjkhash = do
  cjk <- cjkChar
  _ <- char '#'
  _ <- lookAhead $ anySingleBut ' '
  return $ T.pack $ cjk : " #"

hashcjk :: Rule
hashcjk = do
  _ <- char '#'
  _ <- lookAhead $ anySingleBut ' '
  cjk <- cjkChar
  return $ T.pack $ "# " ++ [cjk]

-- operators
cjkOPTan :: Rule
cjkOPTan = do
  cjk <- cjkChar
  opt <- oneOf ("+-=*/&|<>%" :: [Char])
  an <- alphanumericChar
  return $ T.pack [cjk, ' ', opt, ' ', an]

anOPTcjk :: Rule
anOPTcjk = do
  an <- alphanumericChar
  opt <- oneOf ("+-=*/&|<>%" :: [Char])
  cjk <- cjkChar
  return $ T.pack [an, ' ', opt, ' ', cjk]

-- slash/bracket rules are not implemented

-- CJK and alphanumeric without space

cjkans :: Rule
cjkans = do
  cjk <- cjkChar
  _ <- lookAhead (alphanumericChar <|> oneOf ("@$%^&*-+\\=|/" :: [Char]))
  return $ T.pack [cjk, ' ']

anscjk :: Rule
anscjk = do
  an <- alphanumericChar <|> oneOf ("~!$%^&*-+\\=|;:,./?" :: [Char])
  _ <- lookAhead cjkChar
  return $ T.pack [an, ' ']

-- rule set, the order matters
recursiveRules :: RuleSet
recursiveRules =
  [ fullwidthCJKsymCJK,
    fullwidthCJKsym
  ]

onepassRules :: RuleSet
onepassRules =
  [ dotsCJK,
    fixCJKcolAN,
    cjkquote,
    quoteCJK,
    fixQuote,
    cjkpossessivequote,
    -- singlequoteCJK,
    fixPossessivequote,
    hashANSCJKhash,
    cjkhash,
    -- hashcjk,
    anscjk,
    cjkans,
    empty -- a dummy rule
  ]

pangu :: Text -> Text
pangu input = applyRules onepassRules $ applyRulesRecursively recursiveRules input