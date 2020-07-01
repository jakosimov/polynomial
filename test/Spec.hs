import Numeric.Polynomial
import Test.Hspec (hspec, describe, it, shouldBe)
import Data.List (findIndex)

main :: IO ()
main = hspec $ do
  describe "Polynomial arithmitic" $
    it "can handle nested addition, subtraction and multiplication" $
      (Term 1 1 - Term 1 0) * (Term 1 1 - Term 1 0) `shouldBe` Term 1 2 - Term 2 1 + Term 1 0
    
  describe "show Polynomial" $ do

    it "can handle simple expressions" $ do
      show ((Term 1 1 + Term 1 0) * (Term 1 1 + Term 1 0)) `shouldBe`  "x² + 2x + 1"
      show (Term 2 0 * (Term 4 2 + Term 1 1)) `shouldBe` "8x² + 2x"

    it "can handle frationals" $
      show (Term (2/3) 0 * (Term 4 2 + Term 1 1)) `shouldBe` "(8/3)x² + (2/3)x"


  describe "normalize Polynomial" $
    it "removes leading coefficient" $
      normalize (Term 2 2 + Term 2 1 + 1) `shouldBe` (Term 1 2 + Term 1 1 + Term (1/2) 0)

  describe "Polynomial.quotientRemainder" $ do
    let f = Term 1 4 + Term 2 3 + Term 11 2 + Term 2 1 + 10
        g = Term 1 3 - Term 1 2 - Term 4 1 - 6

    it "returns correct quotient" $
      f </> g `shouldBe` Term 1 1 + 3

    it "returns correct remainder" $
      f <%> g `shouldBe` Term 18 2 + Term 20 1 + 28


  describe "Polynomial.greatestcommondivisor" $
    it "returns correct greatest common divisor" $ do
      let f = Term 4 4 + Term 5 3 + Term 8 2 + Term 12 1 + 5
          g = Term 9 6 + Term 9 5 + Term 3 3 + Term 3 2 + Term 19 1 + 19
      greatestCommonDivisor f g `shouldBe` (Term 1 1 + 1)

      let f' = Term 3 5 + Term 6 4 + Term 8 3 + Term 13 2 + Term 11 1 + 3
          g' = Term 2 4 + Term 10 3 + Term 17 2 + Term 12 1 + 3
      greatestCommonDivisor f' g' `shouldBe` (Term 1 2 + Term 2 1 + 1)

  describe "PolynomialRatio.over" $
    it "simplifies expressions correctly" $
      let p      = Term 3 3 + Term 4 2 - Term 8 1 - 3
          q      = Term 1 4 + Term 1 3 + Term 3 1 - 9
          result = (Term 3 1 + 1) `over` (Term 1 2 + 3)
      in p `over` q `shouldBe` result

