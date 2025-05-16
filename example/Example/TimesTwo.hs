{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Example.TimesTwo where

import Numeric.Natural (Natural)

fromNatural :: Natural -> Natural
fromNatural = (* 2)

pattern FromNatural :: Natural -> Natural
pattern FromNatural x <- ((`divMod` 2) -> (x, 0))
