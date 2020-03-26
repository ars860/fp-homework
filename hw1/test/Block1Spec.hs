module Block1Spec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)
import Test.QuickCheck (Arbitrary (..), property, listOf)

import qualified Data.Set as Set (fromList)

import Block1.Task1
import Block1.Task2
import Block1.Task3

import Block4.Task3 (toList)

newtype IntList = IntList { getList :: [Int] }
                deriving (Show)

instance Arbitrary IntList where
  arbitrary =
    IntList <$> ((:) <$> arbitrary <*> listOf arbitrary)

treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList (Node left values right) =
  treeToList left ++
  toList values ++
  treeToList right

spec :: Spec
spec = do
  describe "Task1 tests" $ do
    it "Next day after monday" $
      nextDay Mon `shouldBe` Tue
    it "Next day after sunday" $
      nextDay Sun `shouldBe` Mon
    it "Monday + 3 days" $
      afterDays Mon 3 `shouldBe` Thu
    it "Sunday is weekend" $
      isWeekend Sun `shouldBe` True
    it "From Mon to Fri" $
      daysToParty Mon `shouldBe` 4
  describe "Task2 tests" $ do
    it "Simple 2 + 2 test" $
      natToInteger (fromInteger 2 + fromInteger 2) `shouldBe` 4
    it "Mul tests" $ do
      natToInteger (fromInteger 3 * fromInteger 5) `shouldBe` 15
      natToInteger (fromInteger 10 * fromInteger 11) `shouldBe` 110
    it "Sub tests" $ do
      natToInteger (fromInteger 10 - fromInteger 5) `shouldBe` 5
      natToInteger (fromInteger 33 - fromInteger 14) `shouldBe` 19
    it "convert tests" $ do
      fromInteger 4 `shouldBe` S (S (S (S Z)))
      natToInteger (S $ S $ S $ S $ S $ S Z) `shouldBe` 6
    it "equal test" $ do
      S (S (S Z)) == S (S (S Z)) `shouldBe` True
      S (S (S Z)) == S (S Z) `shouldBe` False
    it "comparsion tests" $ do
      S (S (S Z)) > S (S Z) `shouldBe` True
      S (S (S Z)) < S (S (S (S Z))) `shouldBe` True
  describe "Task2 hard version test" $ do
    it "isEven test" $ do
      isEven (S (S (S Z))) `shouldBe` False
      isEven (S (S Z)) `shouldBe` True
    it "natDiv test" $ do
      S (S (S (S Z))) `natDiv` S (S Z) `shouldBe` S (S Z)
      (fromInteger 11) `natDiv` (fromInteger 3)
        `shouldBe`
        (fromInteger 3)
    it "natMod test" $ do
      S (S (S (S Z))) `natMod` S (S Z) `shouldBe` Z
      (fromInteger 11) `natMod` (fromInteger 3)
        `shouldBe`
        (fromInteger 2)
  describe "Task3 test" $ do
    it "size and from list test" $ do
      size (fromList [1, 4, 6, 2, 3, 6]) `shouldBe` 6
    it "remove test 1" $ do
      remove
        (fromList $ reverse [ 2 , 1 , 15, 16
                            , 14, 13, 9 , 11
                            , 10, 12, 7 , 8
                            , 3 , 5 , 4 , 6
                            ])
        9
      `shouldBe`
      fromList (reverse [ 2 , 1 , 15, 16
                        , 14, 13, 10, 11
                        , 12, 7 , 8 , 3
                        , 5 , 4 , 6
                        ])
    it "remove test 2" $ do
      remove
        (fromList $ reverse [ 2  , 1  , 150, 160
                            , 14 , 13 , 90 , 110
                            , 100, 120, 70 , 80
                            , 30 , 50 , 40 , 60
                            ])
        14
      `shouldBe`
      fromList (reverse [ 2  , 1 , 150, 160
                        , 30 , 13, 90 , 110
                        , 120, 100, 70, 80
                        , 50 , 40 , 60])
    it "remove head" $ do
      remove (fromList [3, 2, 1]) 2 `shouldBe` fromList [3, 1]
    it "remove from list in node" $ do
      remove (fromList [3, 3, 3]) 3 `shouldBe` fromList [3, 3]
    it "remove leaf" $ do
      remove (fromList [3, 1, 2]) 3 `shouldBe` fromList [1, 2]
    it "insert" $ do
      insert (fromList [3, 1, 2]) 4 `shouldBe` fromList [4, 3, 1, 2]
    it "remove property" $ property $
      \(IntList list) ->
        Set.fromList (treeToList (remove (fromList list) (head list)))
        `shouldBe`
        Set.fromList (tail list)
