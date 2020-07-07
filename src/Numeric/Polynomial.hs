module Numeric.Polynomial
  ( Polynomial()
  , degree
  , nullPolynomial
  , termsOf
  , quotientRemainder
  , (</>)
  , (<%>)
  , term
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
  ( Polynomial()
  , degree
  , nullPolynomial
  , termsOf
  , quotientRemainder
  , term
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

