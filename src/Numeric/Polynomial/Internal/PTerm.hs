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
          asExponent char
            | char == '1'            = '\185'
            | char == '2'            = '\178'
            | char == '3'            = '\179'
            | char `elem` ['4'..'9'] = chr $ ord char + 8256
            | otherwise              = char

instance Ord PTerm where
  compare (PTerm c d) (PTerm c' d')
    | d == d'   = compare c c'
    | otherwise = compare d d'

multiply :: PTerm -> PTerm -> PTerm
multiply (PTerm c d) (PTerm c' d') = PTerm (c*c') (d+d')

termSignum :: Num a => PTerm -> a
termSignum (PTerm c _)
  | c > 0     = 1
  | c < 0     = -1
  | otherwise = 0

negateTerm :: PTerm -> PTerm
negateTerm (PTerm c d) = PTerm (-c) d
