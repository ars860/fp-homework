module Block3Spec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)

import Block3.Task1
import Block3.Task2

data Abcdef = A | B | C | D | E | F deriving (Show, Eq)

spec :: Spec
spec = do
  describe "Task1 tests" $ do
    it "maybeConcat simple test" $ do
      maybeConcat [Just [1, 2, 3], Nothing, Just [4, 5]]
        `shouldBe`
        [1, 2, 3, 4, 5]
    it "maybeConcat only Nothing test" $ do
      maybeConcat [Nothing, Nothing]
        `shouldBe`
        ([] :: [Int])  -- it is a trick for compilator
    it "maybeConcat empty test" $ do
      maybeConcat [] `shouldBe` ([] :: [Int])
  describe "Task1 hard version tests" $ do
    it "eitherConcat simple test" $ do
      eitherConcat [Right [1, 2, 3], Left [[1]], Right [4, 5]]
        `shouldBe`
        ([[1]], [1, 2, 3, 4, 5])
    it "eitherConcat empty test" $ do
      maybeConcat [] `shouldBe` ([] :: [Int])
  describe "Task2 test" $ do
    it "NonEmpty semigroup associativity test" $ do
      (A :| [B]) <> ((C :| [D]) <> (E :| [F]))
        `shouldBe`
        ((A :| [B]) <> (C :| [D])) <> (E :| [F])
    it "ThisOrThat semigroup associativity tests : This That This" $ do
      (This A) <> ((That B) <> (This C))
        `shouldBe`
        ((This A) <> (That B)) <> (This C)
    it "ThisOrThat semigroup associativity tests : This Both That" $ do
      (This A) <> ((Both B C) <> (That D))
        `shouldBe`
        ((This A) <> (Both B C)) <> (That D)
  describe "Task2 hard version tests" $ do
    it "Name semigroup associativity test" $ do
      Name "haskell" <> (Name "is" <> Name "cool")
        `shouldBe`
        (Name "haskell" <> Name "is") <> Name "cool"
    it "Name monoid rules test" $ do
      Name "haskell" <> mempty
        `shouldBe`
        mempty <> Name "haskell"
      Name "haskell" <> mempty
        `shouldBe`
        Name "haskell"
    it "Endo semigroup associativity test" $ do
      getEndo (Endo (\x -> x + 5) <> (Endo (\x -> x * 5) <> Endo (\x -> x / 5))) 1
        `shouldBe`
        getEndo ((Endo (\x -> x + 5) <> Endo (\x -> x * 5)) <> Endo (\x -> x / 5)) 1
    it "Endo semigroup associativity test" $ do
      getEndo (mempty <> Endo (\x -> x + 5)) 1
        `shouldBe`
        getEndo (Endo (\x -> x + 5) <> mempty) 1
      getEndo (mempty <> Endo (\x -> x + 5)) 1
        `shouldBe`
        getEndo (Endo (\x -> x + 5)) 1

