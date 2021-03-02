module UtilsSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)

import Utils

spec :: Spec
spec = do
  describe "dropAllPointPointThatCan tests" $ do
    it "random test 1" $
      dropAllPointPointThatCan "aba/caba/../baba" `shouldBe` "aba/baba"
    it "random test 2" $
      dropAllPointPointThatCan "1/2/3/../4/5/6/7/8/../../../../../a/1/2/3/../../.." `shouldBe` "1/2/a"
  describe "replaceEndLines tests" $ do
    it "abacabababa test" $
      replaceEndLines "aba\\nbaba\\ncaba\\n" `shouldBe` "aba\nbaba\ncaba\n"
  describe "splitOnSpaceButNotSemicolons tests" $ do
    it "abacabababa test" $ do
      filter (not . null) (splitOnSpaceButNotSemicolons "aba \"caba         baba   \"") `shouldBe` ["aba", "caba         baba   "]
    it "random test" $
      splitOnSpaceButNotSemicolons "asfasf fas\"f asf asfsaf \"asf asf dafahf yasg\"f yasfgyu asgf\"yasgf oyagsf sa"
        `shouldBe`
        ["asfasf","fas","f asf asfsaf ","asf","asf","dafahf","yasg","f yasfgyu asgf","yasgf","oyagsf","sa"]
  describe "addIndent tests" $ do
    it "addIndentToEveryStringExceptFirst abacabababa test" $
      addIndentToEveryStringExceptFirst "indent" "aba\ncaba\nbaba" `shouldBe` "aba\nindentcaba\nindentbaba"
    it "addIndentToEveryString abacabababa test" $
      addIndentToEveryString "indent" "aba\ncaba\nbaba" `shouldBe` "indentaba\nindentcaba\nindentbaba"
  describe "(!!?) tests" $ do
    it "success test" $
      [1,2,3] !!? 1 `shouldBe` Just 2
    it "error test" $
      [1,2,3] !!? 3 `shouldBe` Nothing
  describe "deleteAt tests" $ do
    it "deleteAt success test" $ do
      deleteAt (const "miss") 1 [1,2,3] `shouldBe` Right [1,3]
    it "deleteAt miss test" $ do
      deleteAt (const "miss") 12345 [1,2,3] `shouldBe` Left "miss"
