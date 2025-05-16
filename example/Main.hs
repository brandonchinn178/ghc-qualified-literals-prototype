{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

import Example.HList qualified as HList
import Example.Text qualified as Text
import Example.TimesTwo qualified as TimesTwo

main :: IO ()
main = do
  putStrLn "===== TimesTwo(.)123"
  print TimesTwo.123
  -- -- compile error
  -- print TimesTwo.(-123)

  putStrLn "===== Match TimesTwo"
  case 246 of
    TimesTwo.!123 -> putStrLn "Matches!"
    _ -> error "fails"

  putStrLn "===== Text \"asdf\""
  print Text."asdf"

  putStrLn "===== Match Text"
  case Text."asdf" of
    Text.!"asdf" -> putStrLn "Matches!"
    _ -> error "fails"

  putStrLn "===== HList"
  print (HList.[Just 1, Nothing, Just "abc"] :: HList.HList Maybe '[Int, Bool, String])

  putStrLn "===== testHListExhaustive"
  testHListExhaustive HList.[Just ()]
  testHListExhaustive HList.[Nothing]

testHListExhaustive :: HList.HList Maybe '[()] -> IO ()
testHListExhaustive hl =
  case hl of
    HList.![Just ()] -> putStrLn "Got: Just"
    HList.![Nothing] -> putStrLn "Got: Nothing"
    -- should compile without raising -Wincomplete-patterns
