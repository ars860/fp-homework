module Block5Spec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)

import Block5.Task1

spec :: Spec
spec = do
  describe "Task1 tests" $ do
    it "eval simple test 1" $ do
      eval
        (Binary $ Add
          (Binary $ Sub (Value 1) (Value 2))
          (Binary $ Div (Value 10) (Value 3))
        )
      `shouldBe`
      pure 2
    it "eval simple test 2" $ do
      eval
        (Binary $ Div
          (Binary $ Sub
            (Binary $ Mul (Value 3) (Value 4))
            (Binary $ Pow (Value 3) (Value 4))
          )
          (Binary $ Div (Value 10) (Value 3))
        )
      `shouldBe`
      pure (-23)
    it "eval zero div" $ do
      eval
        (Binary $ Div
          (Binary $ Sub
            (Binary $ Mul (Value 3) (Value 4))
            (Binary $ Pow (Value 3) (Value 4))
          )
          (Binary $ Div (Value 10) (Value 0))
        )
      `shouldBe`
      Left DivisionByZero
    it "eval negative Pow" $ do
      eval
        (Binary $ Div
          (Binary $ Sub
            (Binary $ Mul (Value 3) (Value 4))
            (Binary $ Pow (Value 3) (Value (-12345)))
          )
          (Binary $ Div (Value 10) (Value 2))
        )
      `shouldBe`
      Left NegativePow
