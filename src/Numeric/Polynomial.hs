module Numeric.Polynomial
  ( Polynomial(Term)
  , degree
  , nullPolynomial
  , termsOf
  , CoeffType
  , quotientRemainder
  , (</>)
  , (<%>)
  , greatestCommonDivisor
  , normalize
  ) where

import Data.List (foldl', sort)
import Data.Char (ord, chr)
import Data.Ratio (denominator, numerator)

type CoeffType = Rational

data PTerm = PTerm CoeffType Int deriving Eq

showRational :: Rational -> String
showRational q
  | d == 1    = show n
  | otherwise = "(" ++ show n ++ "/" ++ show d ++ ")"
  where d = denominator q
        n = numerator q

instance Show PTerm where
  show (PTerm c d) = prefix ++ postfix
    where prefix  = if c == 1 && d /= 0 then "" else showRational c
          postfix
            | d == 0     = ""
            | d == 1     = "x"
            | otherwise  = "x" ++ exponent
            where exponent     = map asExponent (show d)
                  asExponent c
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

data Polynomial = Term CoeffType Int
                | Expr [PTerm]

instance Show Polynomial where
  show p =
    case getTerms p of
      []   -> ""
      p:ps -> show p ++ (ps >>= toString)
    where toString term
            | termSignum term < 1 = " - " ++ show (negateTerm term)
            | otherwise           = " + " ++ show term

instance Eq Polynomial where
  a == b = getTerms a == getTerms b

instance Ord Polynomial where
  compare a b = compare (getTerms a) (getTerms b)

getTerms :: Polynomial -> [PTerm]
getTerms (Term c d) = reduce [PTerm c d]
getTerms (Expr as)  = reduce as

reduce :: [PTerm] -> [PTerm]
reduce as = filter (not . isZero)
            $ foldl' step []
            $ sort as
  where isZero (PTerm c d) = c == 0
        step [] a = [a]
        step ((PTerm c d):xs) (PTerm c' d')
          | d == d'   = PTerm (c+c') d : xs
          | otherwise = (PTerm c' d') : (PTerm c d) : xs

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
      t:_ -> termSignum t
      
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
  | qs == []            = error "Division by zero"
  | degree p < degree q = (nullPolynomial, p)
  | otherwise =
      let ((Term c d):_)   = ps
          ((Term c' d'):_) = qs
          multiplier       = (Term (c/c') (d - d'))
          p'               = p - multiplier * q
          (quot, rem)      = quotientRemainder p' q
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
  | degree q <= 0       = p
  | otherwise           = greatestCommonDivisor q (normalize $ p <%> q)

