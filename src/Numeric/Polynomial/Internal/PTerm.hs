module Numeric.Polynomial.Internal.PTerm where

import Data.Char (ord, chr)
import qualified Data.Ratio as Ratio (denominator, numerator)

data PTerm = PTerm Rational Int deriving Eq

showRational :: Rational -> String
showRational q
  | d == 1    = show n
  | otherwise = "(" ++ show n ++ "/" ++ show d ++ ")"
  where d = Ratio.denominator q
        n = Ratio.numerator q

instance Show PTerm where
  show (PTerm c d) = prefix ++ postfix
    where prefix
            | c == 1 && d /= 0    = ""
            | c == (-1) && d /= 0 = "-"
            | otherwise           = showRational c
          postfix
            | d == 0     = ""
            | d == 1     = "x"
            | otherwise  = "x" ++ exponent
          exponent = map asExponent (show d)
          asExponent c
            | c == '1'            = '\185'
            | c == '2'            = '\178'
            | c == '3'            = '\179'
            | c `elem` ['4'..'9'] = chr $ ord c + 8256
            | otherwise           = c

instance Ord PTerm where
  compare (PTerm c d) (PTerm c' d')
    | d == d'   = compare c c'
    | otherwise = compare d d'

multiply :: PTerm -> PTerm -> PTerm
multiply (PTerm c d) (PTerm c' d') = PTerm (c*c') (d+d')

termSignum (PTerm c _)
  | c > 0     = 1
  | c < 0     = -1
  | otherwise = 0

negateTerm (PTerm c d) = PTerm (-c) d
