{-# LANGUAGE ScopedTypeVariables #-}

module Block2Spec (spec) where

import Data.Monoid ( Sum (..), getSum
                   , Endo (..), appEndo
                   )

import qualified Data.Set as Set

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)
import Test.QuickCheck ( Arbitrary (..)
                       , elements
                       , Gen
                       , property
                       , listOf
                       )

import Block1.Task3 (fromList, size)
import Block2.Task1 ()
import Block2.Task2

genAbcChar :: Gen Char
genAbcChar = elements ['a', 'b', 'c']

genAbcString :: Gen String
genAbcString = listOf genAbcChar

newtype AbcString = AbcString {getString :: String}
                  deriving (Show)

instance Arbitrary AbcString where
  arbitrary = AbcString <$> genAbcString

spec :: Spec
spec = do
  describe "Task1 property tests" $ do
    it "tree as set test" $ property $
      \(list :: [Int]) ->
        Set.fromList (foldr (\a b -> a : b) [] (fromList list))
        `shouldBe`
        Set.fromList list
    it "tree sum test" $ property $
      \(list :: [Int]) ->
        foldr (+) 0 (fromList list)
        `shouldBe`
        foldr (+) 0 list
    it "tree mul endo test" $ property $
      \(list :: [Integer]) ->
        foldr (*) 1 (fromList list)
        `shouldBe`
        appEndo (foldMap (Endo . (*)) (fromList list)) 1
    it "tree size test" $ property $
      \(list :: [Int]) ->
        (getSum . foldMap (Sum . const 1)) (fromList list)
        `shouldBe`
        size (fromList list)
  describe "Task2 tests" $ do
    it "splitOn simple test 1" $ do
      splitOn '.' "ha.ha.ha.skell"
        `shouldBe`
        ["ha", "ha", "ha", "skell"]
    it "splitOn simple test 2" $ do
      splitOn 'o' "abaocabaobaba"
        `shouldBe`
        ["aba", "caba", "baba"]
    it "splitOn empty test" $ do
      splitOn '.' ""
        `shouldBe`
        "" : []
  describe "Task2 hard version tests" $ do
    it "joinWith simple test 1" $ do
      joinWith '.' (["ha", "ha", "ha", "skell"])
        `shouldBe`
        "ha.ha.ha.skell"
    it "joinWith simple test 2" $ do
      joinWith 'o' (["aba", "caba", "baba"])
        `shouldBe`
        "abaocabaobaba"
    it "joinWith empty test" $ do
      joinWith '.' ("" : [])
        `shouldBe`
        ""
    it "joinWith . splitOn property test" $ property $
      \(s :: AbcString) ->
        (joinWith 'a' . splitOn 'a') (getString s) == (getString s)


