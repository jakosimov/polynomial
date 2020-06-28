import Numeric.Polynomial
import Data.List (findIndex)

basicTests :: [Bool]
basicTests =
  [ show ((Term 1 1 + Term 1 0) * (Term 1 1 + Term 1 0)) == "x² + 2x + 1"
  , (Term 1 1 + Term 1 0) * (Term 1 1 + Term 1 0) == Term 1 2 + Term 2 1 + Term 1 0
  , show ((Term 2 0) * (Term 4 2 + Term 1 1)) == "8x² + 2x"
  , show ((Term (2/3) 0) * (Term 4 2 + Term 1 1)) == "(8/3)x² + (2/3)x"
  , normalize (Term 2 2 + Term 2 1 + 1) == (Term 1 2 + Term 1 1 + Term (1/2) 0)
  , let f = Term 1 4 + Term 2 3 + Term 11 2 + Term 2 1 + 10
        g = Term 1 3 - Term 1 2 - Term 4 1 - 6
    in quotientRemainder f g == (Term 1 1 + 3, Term 18 2 + Term 20 1 + 28)
  , let f = Term 4 4 + Term 5 3 + Term 8 2 + Term 12 1 + 5
        g = Term 9 6 + Term 9 5 + Term 3 3 + Term 3 2 + Term 19 1 + 19
    in greatestCommonDivisor f g == (Term 1 1 + 1)
  , let f = Term 3 5 + Term 6 4 + Term 8 3 + Term 13 2 + Term 11 1 + 3
        g = Term 2 4 + Term 10 3 + Term 17 2 + Term 12 1 + 3
    in greatestCommonDivisor f g == (Term 1 2 + Term 2 1 + 1)
  ]

main :: IO ()
main =
  do let result = basicTests
     case findIndex not result of
       Nothing -> putStrLn "All tests passed!"
       Just n  -> putStrLn $ " Test [" ++ show n ++ "] failed!"

