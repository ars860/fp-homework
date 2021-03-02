module Task1Spec where

import Test.Hspec ( Spec, describe, it )
import Test.Hspec.Expectations ( shouldBe )

import Task1 ( Point (..), perimeter, square )

spec :: Spec
spec = do
  describe "square and perimeter tests" $ do
    it "sqaure/perimeter of: (0,0) (1,0) (1,1) (0,1)" $ do
      let quadrate = [Point 0 0, Point 1 0, Point 1 1, Point 0 1]
      square quadrate `shouldBe` 1
      perimeter quadrate `shouldBe` 4
    it "same with triangle: (0,0) (3,6) (6,0)" $ do
      let triangle = [Point 0 0, Point 6 3, Point 0 6]
      square triangle `shouldBe` 18
      perimeter triangle `shouldBe` 6 * (1 + sqrt 5)

