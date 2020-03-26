{-# LANGUAGE ScopedTypeVariables #-}

module Block4Spec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)

import Test.QuickCheck

import Block2.Task2 (joinWith)

import Block4.Task1

newtype IntString = IntString { getString :: String }
                  deriving (Show)

instance Arbitrary IntString where
  arbitrary =
    IntString
    <$> ((foldMap ((:) ' '))
      <$> (listOf $ show
        <$> (arbitrary :: Gen Int)))

spec :: Spec
spec = do
  describe "Task1 tests" $ do
    it "stringSum simple test" $ do
      stringSum "1 2 3 4 5"
        `shouldBe`
        Just 15
    it "stringSum failing" $ do
      stringSum "1 2 three 4 5"
        `shouldBe`
        Nothing
  describe "Task1 property tests" $ do
    it "stringSum property tests" $ property $
      \(IntString intList) ->
        stringSum intList
          `shouldBe`
          Just (sum ((read :: String -> Int) <$> words intList))
