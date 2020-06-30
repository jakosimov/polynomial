module Numeric.Polynomial.Internal.PolynomialRatio where

import Numeric.Polynomial.Internal.PolynomialType

data PolynomialRatio = PolynomialRatio Polynomial Polynomial

simplify :: PolynomialRatio -> PolynomialRatio
simplify (PolynomialRatio p q) = PolynomialRatio p' q'
  where divisor = greatestCommonDivisor p q
        p'      = p </> divisor
        q'      = q </> divisor

over :: Polynomial -> Polynomial -> PolynomialRatio
p `over` q = simplify (PolynomialRatio p q)

centreText :: Int -> String -> String
centreText n s = padding ++ s ++ padding
  where padding = take paddingLength $ repeat ' '
        paddingLength = (n - length s) `quot` 2

numerator :: PolynomialRatio -> Polynomial
numerator (PolynomialRatio p _) = p

denominator :: PolynomialRatio -> Polynomial
denominator (PolynomialRatio _ q) = q

instance Show PolynomialRatio where
  show (PolynomialRatio p q) = pString' ++ "\n" ++ divisor ++ "\n" ++ qString
    where pString' = centreText length' pString
          qString' = centreText length' qString
          divisor  = take length' $ repeat '-'
          length'  = max (length pString) (length qString)
          pString  = show p
          qString  = show q

instance Eq PolynomialRatio where -- Kanske är fel, kanske inte förkortar bort konstanter
  r == r' = p == p' && q == q'
    where (PolynomialRatio p q)   = simplify r
          (PolynomialRatio p' q') = simplify r'

instance Num PolynomialRatio where
  (PolynomialRatio p q) + (PolynomialRatio p' q') = PolynomialRatio numerator denominator
    where gcd         = greatestCommonDivisor q q'
          multiple    = q' </> gcd
          multiple'   = q </> gcd
          numerator   = p * multiple + p' * multiple'
          denominator = multiple * multiple' * gcd

  negate (PolynomialRatio p q) = PolynomialRatio (negate p) q

  signum (PolynomialRatio p _) = PolynomialRatio (signum p) 1

  (PolynomialRatio p q) * (PolynomialRatio p' q') = PolynomialRatio (p*p') (q*q')

  abs (PolynomialRatio p q) = PolynomialRatio (abs p) q

  fromInteger n = PolynomialRatio (fromInteger n) 1
        
