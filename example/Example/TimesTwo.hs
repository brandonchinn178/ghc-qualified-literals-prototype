module Example.TimesTwo where

import Numeric.Natural (Natural)

fromNatural :: Natural -> Natural
fromNatural = (* 2)

matchNatural :: Natural -> Natural -> Bool
matchNatural = (==) . (* 2)
