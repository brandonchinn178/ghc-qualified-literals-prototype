{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module QualifiedLiterals where

import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (mod)

import QualifiedLiterals.Internal.Parse

processFile :: FilePath -> Text -> IO Text
processFile path file =
  -- (\m -> m >>= \x -> putStrLn (Text.unpack x) >> pure x) $ -- uncomment to debug
  transform <$> parseHaskellFile path file
  where
    transform = addLinePragma . addViewPatterns . process

    -- tells GHC to use the original path in error messages
    addLinePragma src = "{-# LINE 1 " <> showT path <> " #-}\n" <> src

    addViewPatterns src = "{-# LANGUAGE ViewPatterns #-}\n" <> src

process :: HaskellCode -> Text
process (HaskellCode codeChunks) =
  Text.concat . flip map codeChunks $ \case
    UnparsedSource s -> s
    QualifiedNaturalExpr mod x -> parens $ mod <> ".fromNatural " <> showT x
    QualifiedNegativeIntegerExpr mod x -> parens $ mod <> ".fromNegativeInteger " <> showT x
    QualifiedRationalExpr _ _ -> undefined -- TODO
    QualifiedStringExpr _ _ -> undefined -- TODO
    QualifiedListExpr _ _ -> undefined -- TODO
    QualifiedNaturalPat mod x -> parens $ mod <> ".matchNatural " <> showT x <> " -> True"
    QualifiedNegativeIntegerPat _ _ -> undefined -- TODO
    QualifiedRationalPat _ _ -> undefined -- TODO
    QualifiedStringPat _ _ -> undefined -- TODO
    QualifiedListPat _ _ -> undefined -- TODO
    QualifiedListConsPat _ _ -> undefined -- TODO
  where
    parens s = "(" <> s <> ")"

showT :: Show a => a -> Text
showT = Text.pack . show
