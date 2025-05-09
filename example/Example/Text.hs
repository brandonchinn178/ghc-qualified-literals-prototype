module Example.Text where

import Data.Text qualified as T

fromString :: String -> T.Text
fromString = T.pack

matchString :: String -> T.Text -> Bool
matchString = (==) . T.pack
