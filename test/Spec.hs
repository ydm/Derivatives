import Test.Hspec
import Derivatives


main :: IO ()
main = hspec $ do
  describe "Derivatives.derive" $ do
    it "derives constants" $ do
      derive (Const 7) `shouldBe` Const 0
    it "derives variables" $ do
      derive Var `shouldBe` Const 1
    it "derives sums" $ do
      derive (Sum [Const 10, Var]) `shouldBe` Sum [Const 0, Const 1]

  describe "Derivatives.simplify" $ do
    it "simplifies sum of constants" $ do
      simplify (Sum [Const 1, Const 2, Const 3]) `shouldBe` Sum [Const 6]
    it "simplifies sum of constants and a variable" $ do
      simplify (Sum [Const 1, Var, Const 2]) `shouldBe` Sum [Const 3, Var]
