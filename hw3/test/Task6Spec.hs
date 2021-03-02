module Task6Spec where

import Lens.Micro

import Test.Hspec ( Spec, describe, it )
import Test.Hspec.Expectations ( shouldBe )

import Task5 ( FS (..) )
import Task6 ( cd, ls, file )

testingFs :: FS
testingFs = Dir {_name = "abacaba", _contents = [File {_name = ".vcs"},File {_name = "a.txt"},File {_name = "abacaba.iml"},File {_name = "bad.txt"},Dir {_name = "bad_dir", _contents = [Dir {_name = "a", _contents = []},Dir {_name = "not_readable", _contents = []},File {_name = "not_readable.file"},File {_name = "not_writable.file"},File {_name = "some_text.txt"}]},Dir {_name = "out", _contents = []},Dir {_name = "src", _contents = [File {_name = "Test.java"}]},File {_name = "test.test"}]}

spec :: Spec
spec = do
  describe "cd/ls/file tests" $ do
    it "cd test" $ do
      testingFs ^?! cd "bad_dir" ^?! cd "a" `shouldBe` (Dir "a" [])
      testingFs ^? cd "sadasdasdas" `shouldBe` Nothing
    it "ls test" $ do
      testingFs ^.. ls `shouldBe` [".vcs","a.txt","abacaba.iml","bad.txt","bad_dir","out","src","test.test"]
      (File "da") ^.. ls `shouldBe` []
    it "file test" $ do
      testingFs ^? file "test.test" `shouldBe` Just "test.test"
      testingFs ^? file "dsadasdsa" `shouldBe` Nothing

