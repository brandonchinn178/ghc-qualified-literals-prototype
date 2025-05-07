import qualified Data.Text.IO as Text
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)

import QualifiedLiterals

main :: IO ()
main = do
  -- just to be extra sure we don't run into encoding issues
  setLocaleEncoding utf8

  -- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#options-affecting-a-haskell-pre-processor
  fp : input : output : _ <- getArgs

  Text.readFile input >>= processFile fp >>= Text.writeFile output
