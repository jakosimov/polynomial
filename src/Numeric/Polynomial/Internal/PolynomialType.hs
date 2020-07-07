module Numeric.Polynomial.Internal.PolynomialType where

import Numeric.Polynomial.Internal.PTerm
import Data.List (foldl', sort, find)
import qualified Data.Ratio as Ratio (numerator, denominator)
import Data.Ratio ((%))

data Polynomial = Term Rational Int
                | Expr [PTerm]

reduce :: [PTerm] -> [PTerm]
reduce as = filter (not . isZero)
            $ foldl' step []
            $ sort as
  where isZero (PTerm c _) = c == 0
        step [] a = [a]
        step ((PTerm c d):xs) (PTerm c' d')
          | d == d'   = PTerm (c+c') d : xs
          | otherwise = PTerm c' d' : PTerm c d : xs

getTerms :: Polynomial -> [PTerm]
getTerms (Term c d) = reduce [PTerm c d]
getTerms (Expr as)  = reduce as

instance Show Polynomial where
  show p =
    case getTerms p of
      []   -> ""
      a:as -> show a ++ (as >>= toString)
    where toString term
            | termSignum term == (-1) = " - " ++ show (negateTerm term)
            | otherwise               = " + " ++ show term

instance Eq Polynomial where
  a == b = getTerms a == getTerms b

instance Ord Polynomial where
  compare a b = compare (getTerms a) (getTerms b)

instance Num Polynomial where
  a + b = Expr $ reduce (as ++ bs)
    where as = getTerms a
          bs = getTerms b

  negate p = Expr $ map negateTerm (getTerms p)

  fromInteger a = Term (fromInteger a) 0

  a * b = Expr $ reduce $ as >>= multiplyAllBsWith
    where as     = getTerms a
          bs     = getTerms b
          multiplyAllBsWith p = map (multiply p) bs

  signum p =
    case getTerms p of
      []  -> 0
      t:_ -> Term (termSignum t) 0

  abs p
    | signum p == -1 = negate p
    | otherwise      = p

nullPolynomial :: Polynomial
nullPolynomial = Expr []

termsOf :: Polynomial -> [Polynomial]
termsOf = map asPolynomialTerm . getTerms
  where asPolynomialTerm (PTerm c d) = Term c d

degree :: Polynomial -> Int
degree p =
  case getTerms p of
    (PTerm _ d):_ -> d
    []            -> -1

quotientRemainder :: Polynomial -> Polynomial -> (Polynomial, Polynomial)
quotientRemainder p q
  | null qs             = error "Division by zero"
  | degree p < degree q = (nullPolynomial, p)
  | otherwise =
      let Term c d:_   = ps
          Term c' d':_ = qs
          multiplier   = Term (c/c') (d - d')
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
  case getTerms p of
    []            -> nullPolynomial
    (PTerm c _):_ -> Term (1/c) 0 * p

greatestCommonDivisor :: Polynomial -> Polynomial -> Polynomial
greatestCommonDivisor p q
  | degree q > degree p = greatestCommonDivisor q p
  | q == nullPolynomial = p
  | degree q <= 0       = q
  | otherwise           = greatestCommonDivisor q (normalize $ p <%> q)

eval :: Real a => Polynomial -> a -> Rational
eval p x =
  case getTerms p of
    []             -> 0
    PTerm c d : ps -> c * toRational x^d + eval (Expr ps) x

derive :: Polynomial -> Polynomial
derive p =
  case getTerms p of
    []             -> 0
    PTerm c d : ps -> Term (c * toRational d) (d-1) + derive (Expr ps)

isConstant :: Polynomial -> Bool
isConstant p = degree p < 1

termsLowestCommonDenominator :: Polynomial -> Integer
termsLowestCommonDenominator p =
  foldl' lcm 1 $ map denominatorOf (getTerms p)
  where denominatorOf (PTerm c _) = Ratio.denominator c

makeIntegerCoeffs :: Polynomial -> Polynomial
makeIntegerCoeffs p = p * fromInteger (termsLowestCommonDenominator p)

divides :: Integer -> Integer -> Bool
a `divides` b = b `mod` a == 0

findConstantTerm :: Polynomial -> Maybe PTerm
findConstantTerm p =
  find (\(PTerm _ d) -> d == 0) (getTerms p)

findRationalRoots :: Polynomial -> [Rational]
findRationalRoots p =
  case getTerms p' of
    []                     -> [0]
    PTerm highestCoeff _:_ ->
      case findConstantTerm p' of
        Nothing ->
          0 : findRationalRoots (p </> Term 1 1)
        Just (PTerm constantCoeff _) ->
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
