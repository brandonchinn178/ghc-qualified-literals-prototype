import Example.TimesTwo qualified as TimesTwo

main :: IO ()
main = do
  print TimesTwo.123
  -- -- compile error
  -- print TimesTwo.(-123)

  case 246 of
    TimesTwo.!123 -> putStrLn "Matches!"
    _ -> error "fails"
