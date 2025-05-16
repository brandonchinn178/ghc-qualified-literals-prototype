{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Example.Text where

import Data.Text qualified as T

fromString :: String -> T.Text
fromString = T.pack

pattern FromString :: String -> T.Text
pattern FromString s <- (T.unpack -> s)
