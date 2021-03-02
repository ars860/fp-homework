module Task7Spec where

import Lens.Micro

import Test.Hspec ( Spec, describe, it )
import Test.Hspec.Expectations ( shouldBe )

import Task5 ( FS (..) )
import Task6 ( cd, ls )
import Task7 ( changeExtensions, allNames, deleteEmpty, move, getPath )

testingFs :: FS
testingFs = Dir {_name = "abacaba", _contents = [File {_name = ".vcs"},File {_name = "a.txt"},File {_name = "abacaba.iml"},File {_name = "bad.txt"},Dir {_name = "bad_dir", _contents = [Dir {_name = "a", _contents = []},Dir {_name = "not_readable", _contents = []},File {_name = "not_readable.file"},File {_name = "not_writable.file"},File {_name = "some_text.txt"}]},Dir {_name = "out", _contents = []},Dir {_name = "src", _contents = [File {_name = "Test.java"}]},File {_name = "test.test"}]}

spec :: Spec
spec = do
  describe "changeExtensions/allNames/deleteEmpty tests" $ do
    it "changeExtensions test" $ do
      changeExtensions "test" testingFs ^.. ls `shouldBe` [".test","a.test","abacaba.test","bad.test","bad_dir","out","src","test.test"]
    it "allNames test" $ do
      allNames testingFs `shouldBe` ["abacaba",".vcs","a.txt","abacaba.iml","bad.txt","test.test","bad_dir","not_readable.file","not_writable.file","some_text.txt","a","not_readable","out","src","Test.java"]
    it "deleteEmpty tests" $ do
      deleteEmpty (testingFs ^?! cd "bad_dir") "a" ^.. ls `shouldBe` ["not_readable","not_readable.file","not_writable.file","some_text.txt"]
      deleteEmpty testingFs "bad_dir" ^.. ls `shouldBe` testingFs ^.. ls
  describe "Extra task tests" $ do
    it "move/getPath test" $ do
      testingFs ^?! move "bad_dir" . move "a" . getPath `shouldBe` "abacaba/bad_dir/a"
      testingFs ^? move "bad_dir" . move "abacabababa" . getPath `shouldBe` Nothing
