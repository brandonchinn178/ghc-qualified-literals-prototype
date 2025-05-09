{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module QualifiedLiterals where

import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (mod)

import QualifiedLiterals.Internal.Parse

processFile :: FilePath -> Text -> IO Text
processFile path file = do
  code <- parseHaskellFile path file
  -- if path == "example/Main.hs" then print code else pure () -- uncomment to debug
  let code' = transform code
  -- if path == "example/Main.hs" then putStrLn $ Text.unpack code' else pure () -- uncomment to debug
  pure code'
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
    QualifiedStringExpr mod s -> parens $ mod <> ".fromString " <> showT s
    QualifiedListExpr mod srcs ->
      let
        lam = "\\cons_new_var___ nil_new_var___ -> " <> expr
        expr =
          foldr
            (\src acc -> parens src <> " `cons_new_var___` " <> parens acc)
            "nil_new_var___"
            srcs
      in
        parens $ mod <> ".buildList " <> parens lam
    QualifiedNaturalPat mod x -> parens $ mod <> ".matchNatural " <> showT x <> " -> True"
    QualifiedNegativeIntegerPat _ _ -> undefined -- TODO
    QualifiedRationalPat _ _ -> undefined -- TODO
    QualifiedStringPat mod s -> parens $ mod <> ".matchString " <> showT s <> " -> True"
    QualifiedListPat mod srcs ->
      parens $
        foldr
          (\src acc -> mod <> ".ListCons " <> parens src <> " " <> parens acc)
          (mod <> ".ListNil")
          srcs
    QualifiedListConsPat _ _ -> undefined -- TODO
  where
    parens s = "(" <> s <> ")"

showT :: Show a => a -> Text
showT = Text.pack . show
