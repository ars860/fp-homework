module Task5Spec where

import Lens.Micro

import Test.Hspec ( Spec, describe, it )
import Test.Hspec.Expectations ( shouldBe )

import Task5 ( FS (..), contents, name, _Dir, _File )

testingFs :: FS
testingFs = Dir {_name = "abacaba", _contents = [File {_name = ".vcs"},File {_name = "a.txt"},File {_name = "abacaba.iml"},File {_name = "bad.txt"},Dir {_name = "bad_dir", _contents = [Dir {_name = "a", _contents = []},Dir {_name = "not_readable", _contents = []},File {_name = "not_readable.file"},File {_name = "not_writable.file"},File {_name = "some_text.txt"}]},Dir {_name = "out", _contents = []},Dir {_name = "src", _contents = [File {_name = "Test.java"}]},File {_name = "test.test"}]}

spec :: Spec
spec = do
  describe "lenses tests" $ do
    it "name test" $ do
      (Dir "dir" []) ^. name `shouldBe` "dir"
      (File "file") ^. name `shouldBe` "file"
    it "contents test" $ do
      (Dir "dir" [File "a", File "b", File "c"]) ^. contents `shouldBe` [File "a", File "b", File "c"]
      (File "not_a_dir") ^. contents `shouldBe` []
  describe "prisms tests" $ do
    it "_Dir test" $ do
      (Dir "dir" []) ^? _Dir `shouldBe` Just (Dir "dir" [])
      (File "file") ^? _Dir `shouldBe` Nothing
    it "_File test" $ do
      (Dir "dir" []) ^? _File `shouldBe` Nothing
      (File "file") ^? _File `shouldBe` Just (File "file")
