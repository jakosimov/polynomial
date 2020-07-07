module Numeric.Polynomial.Internal.PolynomialType where

import Numeric.Polynomial.Internal.Term
import Data.List (foldl', sort, find)
import qualified Data.Ratio as Ratio (numerator, denominator)
import Data.Ratio ((%))

newtype Polynomial = Polynomial [Term]

reduce :: [Term] -> [Term]
reduce as = filter (not . isZero)
            $ foldl' step []
            $ sort as
  where isZero (Term c _) = c == 0
        step [] a = [a]
        step ((Term c d):xs) (Term c' d')
          | d == d'   = Term (c+c') d : xs
          | otherwise = Term c' d' : Term c d : xs

termsOf :: Polynomial -> [Term]
termsOf (Polynomial as)  = reduce as

term :: Rational -> Int -> Polynomial
term c d = Polynomial [Term c d]

instance Show Polynomial where
  show p =
    case termsOf p of
      []   -> ""
      a:as -> show a ++ (as >>= toString)
    where toString term
            | termSignum term == (-1) = " - " ++ show (negateTerm term)
            | otherwise               = " + " ++ show term

instance Eq Polynomial where
  a == b = termsOf a == termsOf b

instance Ord Polynomial where
  compare a b = compare (termsOf a) (termsOf b)

instance Num Polynomial where
  a + b = Polynomial $ reduce (as ++ bs)
    where as = termsOf a
          bs = termsOf b

  negate p = Polynomial $ map negateTerm (termsOf p)

  fromInteger a = term (fromInteger a) 0

  a * b = Polynomial $ reduce $ as >>= multiplyAllBsWith
    where as     = termsOf a
          bs     = termsOf b
          multiplyAllBsWith p = map (multiply p) bs

  signum p =
    case termsOf p of
      []  -> 0
      t:_ -> term (termSignum t) 0

  abs p
    | signum p == -1 = negate p
    | otherwise      = p

nullPolynomial :: Polynomial
nullPolynomial = Polynomial []

degree :: Polynomial -> Int
degree p =
  case termsOf p of
    (Term _ d):_ -> d
    []           -> -1

quotientRemainder :: Polynomial -> Polynomial -> (Polynomial, Polynomial)
quotientRemainder p q
  | null qs             = error "Division by zero"
  | degree p < degree q = (nullPolynomial, p)
  | otherwise =
      let Term c d:_   = ps
          Term c' d':_ = qs
          multiplier   = term (c/c') (d - d')
          p'           = p - multiplier * q
          (quot, rem)  = quotientRemainder p' q
      in (multiplier + quot, rem)
  where ps = termsOf p
        qs = termsOf q

(</>) :: Polynomial -> Polynomial -> Polynomial
p </> q = fst $ quotientRemainder p q

(<%>) :: Polynomial -> Polynomial -> Polynomial
p <%> q = snd $ quotientRemainder p q

normalize :: Polynomial -> Polynomial
normalize p =
  case termsOf p of
    []           -> nullPolynomial
    (Term c _):_ -> term (1/c) 0 * p

greatestCommonDivisor :: Polynomial -> Polynomial -> Polynomial
greatestCommonDivisor p q
  | degree q > degree p = greatestCommonDivisor q p
  | q == nullPolynomial = p
  | degree q <= 0       = q
  | otherwise           = greatestCommonDivisor q (normalize $ p <%> q)

eval :: Real a => Polynomial -> a -> Rational
eval p x =
  case termsOf p of
    []            -> 0
    Term c d : ps -> c * toRational x^d + eval (Polynomial ps) x

derive :: Polynomial -> Polynomial
derive p =
  case termsOf p of
    []             -> 0
    Term c d : ps -> term (c * toRational d) (d-1) + derive (Polynomial ps)

isConstant :: Polynomial -> Bool
isConstant p = degree p < 1

termsLowestCommonDenominator :: Polynomial -> Integer
termsLowestCommonDenominator p =
  foldl' lcm 1 $ map denominatorOf (termsOf p)
  where denominatorOf (Term c _) = Ratio.denominator c

makeIntegerCoeffs :: Polynomial -> Polynomial
makeIntegerCoeffs p = p * fromInteger (termsLowestCommonDenominator p)

divides :: Integer -> Integer -> Bool
a `divides` b = b `mod` a == 0

findConstantTerm :: Polynomial -> Maybe Term
findConstantTerm p =
  find (\(Term _ d) -> d == 0) (termsOf p)

findRationalRoots :: Polynomial -> [Rational]
findRationalRoots p =
  case termsOf p' of
    []                    -> [0]
    Term highestCoeff _:_ ->
      case findConstantTerm p' of
        Nothing ->
          0 : findRationalRoots (p </> term 1 1)
        Just (Term constantCoeff _) ->
          let highestCoeff' = abs $ Ratio.numerator highestCoeff
              constantCoeff' = abs $ Ratio.numerator constantCoeff
              numers  = filter (`divides` constantCoeff') [1..constantCoeff'] -- Numerators for the solution
              denoms  = filter (`divides` highestCoeff') [1..highestCoeff'] -- Denominators for the solution
          in [q | enum  <- numers
                , denom <- denoms
                , sign  <- [1, -1]
                , let q = sign * (enum % denom)
                , eval p' q == 0
                ]
  where p' = makeIntegerCoeffs p
