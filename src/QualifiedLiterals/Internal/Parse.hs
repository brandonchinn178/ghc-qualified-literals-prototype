{-# LANGUAGE OverloadedStrings #-}

module QualifiedLiterals.Internal.Parse (
  HaskellCode (..),
  HaskellCodeChunk (..),
  HaskellCodeSrc,
  parseHaskellFile,
) where

import Control.Monad (mzero, when)
import Data.Char (isDigit, isSpace, isUpper, toLower)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Prelude hiding (exp, mod)
import Text.Megaparsec
import Text.Megaparsec.Char

newtype HaskellCode = HaskellCode [HaskellCodeChunk]
  deriving (Show)

data HaskellCodeChunk
  = UnparsedSource HaskellCodeSrc
  | QualifiedNaturalExpr ModuleName Integer
  | QualifiedNegativeIntegerExpr ModuleName Integer
  | QualifiedRationalExpr ModuleName Rational
  | QualifiedStringExpr ModuleName String
  | QualifiedListExpr ModuleName [HaskellCodeSrc]
  | QualifiedNaturalPat ModuleName Integer
  | QualifiedNegativeIntegerPat ModuleName Integer
  | QualifiedRationalPat ModuleName Rational
  | QualifiedStringPat ModuleName String
  | QualifiedListPat ModuleName [HaskellCodeSrc]
  | QualifiedListConsPat ModuleName [HaskellCodeSrc]
  deriving (Show)

type ModuleName = Text
type HaskellCodeSrc = Text

parseHaskellFile :: FilePath -> Text -> IO HaskellCode
parseHaskellFile path file =
  case runParser (parseHaskellCode <* eof) path file of
    Left e -> errorWithoutStackTrace $ errorBundlePretty e
    Right parsedFile -> pure parsedFile

{----- Parser -----}

type Parser = Parsec Void Text

parseHaskellCode :: Parser HaskellCode
parseHaskellCode = HaskellCode <$> parseCodeChunks
  where
    parseCodeChunks = concat <$> many parseCodeChunk

    parseCodeChunk =
      choice
        [ try $ (:[]) <$> parseQualifiedLiteral
        , do
            -- consume any leading space
            spaces1 <- takeWhileP Nothing isSpace
            -- consume non-uppercase characters
            src <- takeWhileP Nothing (not . isUpper)
            -- consume any trailing space
            spaces2 <- takeWhileP Nothing isSpace
            -- if we didn't consume anything, explicitly fail
            when (Text.null spaces1 && Text.null src && Text.null spaces2) mzero
            -- then stop, so that we can check for any qualified literals
            pure [UnparsedSource $ spaces1 <> src <> spaces2]
        , do
            -- if we get to this point, the first character is uppercase, but it's not
            -- a qualified literal. consume that uppercase character and all non-uppercase
            -- letters so we can progress
            c <- takeP Nothing 1
            cs <- takeWhileP Nothing (not . isUpper)
            pure [UnparsedSource $ c <> cs]
        ]

parseQualifiedLiteral :: Parser HaskellCodeChunk
parseQualifiedLiteral = do
  mod <- Text.intercalate "." <$> many parseModDot
  isPat <- isJust <$> optional (char '!')
  choice . map try $
    [ do
        x <- parseInt
        pure $ if isPat then QualifiedNaturalPat mod x else QualifiedNaturalExpr mod x
    , do
        x <- between (char '(') (char ')') parseInt
        pure $ if isPat then QualifiedNaturalPat mod x else QualifiedNaturalExpr mod x
    , do
        x <- between (char '(') (char ')') (char '-' *> parseInt)
        pure $ if isPat then QualifiedNegativeIntegerPat mod x else QualifiedNegativeIntegerExpr mod x
    , do
        x <- between (char '(') (char ')') $ do
          prefix <- optOrEmpty (string "-")
          intDigits <- takeWhile1P Nothing isDigit
          fracDigits <- optOrEmpty $ Text.cons <$> char '.' <*> takeWhile1P Nothing isDigit
          exp <- optOrEmpty $ do
            e <- string "e" <|> string "E"
            neg <- optOrEmpty $ string "-"
            ds <- takeWhile1P Nothing isDigit
            pure $ e <> neg <> ds
          pure $ readT (prefix <> intDigits <> fracDigits <> exp)
        pure $ if isPat then QualifiedRationalPat mod x else QualifiedRationalExpr mod x
    -- TODO: strings
    -- TODO: lists
    ]
  where
    optOrEmpty = fmap (fromMaybe "") . optional
    parseInt =
      choice . map try $
        [ do
            prefix <- string "0x"
            digits <- takeWhile1P Nothing (\c -> isDigit c || toLower c `isin` "abcdef")
            pure $ readT (prefix <> digits)
        , do
            prefix <- string "0o"
            digits <- takeWhile1P Nothing (\c -> c `isin` "01234567")
            pure $ readT (prefix <> digits)
        , do
            prefix <- string "0b"
            digits <- takeWhile1P Nothing (\c -> c `isin` "01")
            pure $ readT (prefix <> digits)
        , do
            digits <- takeWhile1P Nothing isDigit
            pure $ readT digits
        ]

    isin :: Char -> [Char] -> Bool
    isin = elem

    readT :: Read a => Text -> a
    readT = read . Text.unpack

parseModDot :: Parser Text
parseModDot = cons <$> upperChar <*> many identChar <* string "."
  where
    identChar = alphaNumChar <|> char '_'
    cons c cs = Text.pack (c : cs)
