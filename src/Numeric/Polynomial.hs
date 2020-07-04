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
  , eval
  , findRationalRoots
  , derive
  , makeIntegerCoeffs
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
  , eval
  , findRationalRoots
  , derive
  , makeIntegerCoeffs
  )

import Numeric.Polynomial.Internal.PolynomialRatio
  ( PolynomialRatio
  , over
  , numerator
  , denominator
  )

