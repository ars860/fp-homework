module Task3Spec where

import Data.Maybe ( isNothing )

import Test.Hspec ( Spec, describe, it )
import Test.Hspec.Expectations ( shouldBe )

import Task3 ( HalyavaScriptExpr (..), ArithExpr (..), interpretAndUnwrap
             , callHalyavaFunction1, halyavaOddPow, callHalyavaFunction2
             , interpretHalyavaScript, (@=)
             )

spec :: Spec
spec = do
  describe "Interpret tests" $ do
    it "1 + 1" $ do
      (interpretAndUnwrap (Return $ AENum 1 `AEPlus` AENum 1) :: Int) `shouldBe` 2
    it "increment function" $ do
      (callHalyavaFunction1 (Fun1 (\x -> Return (AEVar x `AEPlus` AENum 1))) :: Int -> Maybe Int) 13 `shouldBe` Just 14
    it "no return" $ do
      isNothing (interpretHalyavaScript (WithVar 0 (\v -> v @= AEVar v `AEPlus` AENum 123))) == True
    it "halyavaOddPow test" $ do
      (callHalyavaFunction2 halyavaOddPow :: Int -> Int -> Maybe String) 6 5 `shouldBe` Just "6 ^ 5 = 1 * 6 * 6 * 6 * 6 * 6 |!!!| Result is not odd and greater then 1000! Fixing! |!!!| / 2 / 2 / 2 / 2 / 2 = 243"
