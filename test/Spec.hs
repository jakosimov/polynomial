import Numeric.Polynomial
import Test.Hspec (hspec, describe, it, shouldBe, Expectation)
import Data.List (findIndex, sort)

rationalRootTest :: [Rational] -> Expectation
rationalRootTest solutions =
  sort (findRationalRoots polynomial) `shouldBe` sort solutions
  where factors    = map (\c -> term 1 1 - term c 0) solutions
        polynomial = product factors

main :: IO ()
main = hspec $ do
  describe "Polynomial arithmitic" $
    it "can handle nested addition, subtraction and multiplication" $
      (term 1 1 - term 1 0) * (term 1 1 - term 1 0) `shouldBe` term 1 2 - term 2 1 + term 1 0
    
  describe "show Polynomial" $ do

    it "can handle simple expressions" $ do
      show ((term 1 1 + term 1 0) * (term 1 1 + term 1 0)) `shouldBe`  "x² + 2x + 1"
      show (term 2 0 * (term 4 2 + term 1 1)) `shouldBe` "8x² + 2x"

    it "can handle frationals" $
      show (term (2/3) 0 * (term 4 2 + term 1 1)) `shouldBe` "(8/3)x² + (2/3)x"


  describe "normalize Polynomial" $
    it "removes leading coefficient" $
      normalize (term 2 2 + term 2 1 + 1) `shouldBe` (term 1 2 + term 1 1 + term (1/2) 0)

  describe "Polynomial.quotientRemainder" $ do
    let f = term 1 4 + term 2 3 + term 11 2 + term 2 1 + 10
        g = term 1 3 - term 1 2 - term 4 1 - 6

    it "returns correct quotient" $
      f </> g `shouldBe` term 1 1 + 3

    it "returns correct remainder" $
      f <%> g `shouldBe` term 18 2 + term 20 1 + 28


  describe "Polynomial.greatestcommondivisor" $
    it "returns correct greatest common divisor" $ do
      let f = term 4 4 + term 5 3 + term 8 2 + term 12 1 + 5
          g = term 9 6 + term 9 5 + term 3 3 + term 3 2 + term 19 1 + 19
      greatestCommonDivisor f g `shouldBe` (term 1 1 + 1)

      let f' = term 3 5 + term 6 4 + term 8 3 + term 13 2 + term 11 1 + 3
          g' = term 2 4 + term 10 3 + term 17 2 + term 12 1 + 3
      greatestCommonDivisor f' g' `shouldBe` (term 1 2 + term 2 1 + 1)

  describe "PolynomialRatio.over" $
    it "simplifies expressions correctly" $
      let p      = term 3 3 + term 4 2 - term 8 1 - 3
          q      = term 1 4 + term 1 3 + term 3 1 - 9
          result = (term 3 1 + 1) `over` (term 1 2 + 3)
      in p `over` q `shouldBe` result

  describe "Polynomial.eval" $
    it "evalutate polynomials correctly" $
      let p = term 3 3 + term 8 2 - term 2 1 + term 99 0
          x = 2
      in eval p x `shouldBe` 151

  describe "Polynomial.findRationalRoots" $
    it "finds rational roots" $
      rationalRootTest [3/2, (-9)/2, 11/17, 101/49] -- These could be auto-generated

