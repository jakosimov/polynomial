module Numeric.Polynomial
  ( Polynomial(Term)
  , degree
  , nullPolynomial
  , termsOf
  , quotientRemainder
  , (</>)
  , (<%>)
  , greatestCommonDivisor
  , normalize
  , PolynomialRatio
  , over
  , numerator
  , denominator
  ) where

import Numeric.Polynomial.Internal.PolynomialType
  ( Polynomial(Term)
  , degree
  , nullPolynomial
  , termsOf
  , quotientRemainder
  , (</>)
  , (<%>)
  , greatestCommonDivisor
  , normalize
  )

import Numeric.Polynomial.Internal.PolynomialRatio
  ( PolynomialRatio
  , over
  , numerator
  , denominator
  )

