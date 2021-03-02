module FileSystemSpec (spec) where

import qualified Data.ByteString.UTF8 as B8 (fromString)
import Data.Either (fromRight, isLeft, isRight)
import Data.Maybe (fromJust, isJust, isNothing)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe, shouldSatisfy)

import FileSystem

-- .
-- ├── a.txt
-- ├── bad.txt   <- not readable
-- ├── bad_dir
-- │   ├── not_readable [error opening dir]  <- a b c dirs inside
-- │   ├── not_readable.file
-- │   ├── not_searchable  <- a b c dirs inside
-- │   ├── not_writable
-- │   │   ├── a
-- │   │   ├── b
-- │   │   └── c
-- │   ├── not_writable.file
-- │   ├── some_text.txt
-- │   └── unaccessible.file
-- ├── small_dir  <- all rw here
-- │   ├── a
-- │   ├── abacaba.baba
-- │   └── b
-- └── test.test
--
-- huge hardcoded directory for testing
testingFs :: Directory
testingFs = read "Directory {dpath = \"/resources\", children = fromList [(\"a.txt\",FSFile (File {fpath = \"/resources/a.txt\", fcontent = \"aba 1234 baba 5678 eaba 9876asfdasfsaf\", fpermissions = Permissions {readable = True, writable = True, executable = False, searchable = False}, faccess = Just 2020-05-06 16:06:06.1304759 UTC, fmodification = Just 2020-05-05 15:05:22.7404123 UTC, fsizeIfUnaccessible = Nothing, fvcs = Nothing})),(\"bad.txt\",FSFile (File {fpath = \"/resources/bad.txt\", fcontent = \"\", fpermissions = Permissions {readable = False, writable = True, executable = False, searchable = False}, faccess = Just 2020-05-06 12:24:26.7587309 UTC, fmodification = Just 2020-05-05 15:05:22.7424129 UTC, fsizeIfUnaccessible = Nothing, fvcs = Nothing})),(\"bad_dir\",FSDirectory (Directory {dpath = \"/resources/bad_dir\", children = fromList [(\"not_readable\",FSDirectory (Directory {dpath = \"/resources/bad_dir/not_readable\", children = fromList [], dpermissions = Permissions {readable = False, writable = True, executable = False, searchable = True}, daccess = Just 2020-05-06 16:04:11.2990967 UTC, dmodification = Just 2020-05-06 12:30:20.5428392 UTC})),(\"not_readable.file\",FSFile (File {fpath = \"/resources/bad_dir/not_readable.file\", fcontent = \"\", fpermissions = Permissions {readable = False, writable = True, executable = False, searchable = False}, faccess = Just 2020-05-06 12:24:26.6344122 UTC, fmodification = Just 2020-05-05 15:05:22.7424129 UTC, fsizeIfUnaccessible = Nothing, fvcs = Nothing})),(\"not_searchable\",FSDirectory (Directory {dpath = \"/resources/bad_dir/not_searchable\", children = fromList [], dpermissions = Permissions {readable = True, writable = True, executable = False, searchable = False}, daccess = Just 2020-05-06 16:04:11.3000966 UTC, dmodification = Just 2020-05-06 15:56:20.4341743 UTC})),(\"not_writable\",FSDirectory (Directory {dpath = \"/resources/bad_dir/not_writable\", children = fromList [(\"a\",FSDirectory (Directory {dpath = \"/resources/bad_dir/not_writable/a\", children = fromList [], dpermissions = Permissions {readable = True, writable = True, executable = False, searchable = True}, daccess = Just 2020-05-06 16:06:06.1344756 UTC, dmodification = Just 2020-05-06 12:32:44.9032457 UTC})),(\"b\",FSDirectory (Directory {dpath = \"/resources/bad_dir/not_writable/b\", children = fromList [], dpermissions = Permissions {readable = True, writable = True, executable = False, searchable = True}, daccess = Just 2020-05-06 16:06:06.1354756 UTC, dmodification = Just 2020-05-06 12:32:46.4627134 UTC})),(\"c\",FSDirectory (Directory {dpath = \"/resources/bad_dir/not_writable/c\", children = fromList [], dpermissions = Permissions {readable = True, writable = True, executable = False, searchable = True}, daccess = Just 2020-05-06 16:06:06.1354756 UTC, dmodification = Just 2020-05-06 12:32:47.9668723 UTC}))], dpermissions = Permissions {readable = True, writable = False, executable = False, searchable = True}, daccess = Just 2020-05-06 16:06:06.1334756 UTC, dmodification = Just 2020-05-06 12:32:47.9668723 UTC})),(\"not_writable.file\",FSFile (File {fpath = \"/resources/bad_dir/not_writable.file\", fcontent = \"dabababa\\n\", fpermissions = Permissions {readable = True, writable = False, executable = False, searchable = False}, faccess = Just 2020-05-06 16:06:06.1364756 UTC, fmodification = Just 2020-05-05 14:29:24.3868707 UTC, fsizeIfUnaccessible = Nothing, fvcs = Nothing})),(\"some_text.txt\",FSFile (File {fpath = \"/resources/bad_dir/some_text.txt\", fcontent = \"asdas\\n\", fpermissions = Permissions {readable = True, writable = True, executable = False, searchable = False}, faccess = Just 2020-05-06 16:06:06.1364756 UTC, fmodification = Just 2020-05-05 15:05:22.7424129 UTC, fsizeIfUnaccessible = Nothing, fvcs = Nothing})),(\"unaccessible.file\",FSFile (File {fpath = \"/resources/bad_dir/unaccessible.file\", fcontent = \"\", fpermissions = Permissions {readable = False, writable = False, executable = False, searchable = False}, faccess = Just 2020-05-06 16:04:08.5195399 UTC, fmodification = Just 2020-05-06 12:31:31.7037496 UTC, fsizeIfUnaccessible = Just 51, fvcs = Nothing}))], dpermissions = Permissions {readable = True, writable = True, executable = False, searchable = True}, daccess = Just 2020-05-06 16:06:06.1314758 UTC, dmodification = Just 2020-05-06 15:56:12.1236035 UTC})),(\"small_dir\",FSDirectory (Directory {dpath = \"/resources/small_dir\", children = fromList [(\"a\",FSDirectory (Directory {dpath = \"/resources/small_dir/a\", children = fromList [], dpermissions = Permissions {readable = True, writable = True, executable = False, searchable = True}, daccess = Just 2020-05-06 16:06:06.1384759 UTC, dmodification = Just 2020-05-06 12:33:49.0628838 UTC})),(\"abacaba.baba\",FSFile (File {fpath = \"/resources/small_dir/abacaba.baba\", fcontent = \"aba caba baba\\n\", fpermissions = Permissions {readable = True, writable = True, executable = False, searchable = False}, faccess = Just 2020-05-06 16:06:06.1394755 UTC, fmodification = Just 2020-05-06 12:34:16.8536457 UTC, fsizeIfUnaccessible = Nothing, fvcs = Nothing})),(\"b\",FSDirectory (Directory {dpath = \"/resources/small_dir/b\", children = fromList [], dpermissions = Permissions {readable = True, writable = True, executable = False, searchable = True}, daccess = Just 2020-05-06 16:06:06.1394755 UTC, dmodification = Just 2020-05-06 12:33:50.6463223 UTC}))], dpermissions = Permissions {readable = True, writable = True, executable = False, searchable = True}, daccess = Just 2020-05-06 16:06:06.1374819 UTC, dmodification = Just 2020-05-06 12:34:01.0294137 UTC})),(\"test.test\",FSFile (File {fpath = \"/resources/test.test\", fcontent = \"1\\n2\\n3\\n\", fpermissions = Permissions {readable = True, writable = True, executable = False, searchable = False}, faccess = Just 2020-05-06 16:06:06.1404758 UTC, fmodification = Just 2020-05-05 15:05:22.7454126 UTC, fsizeIfUnaccessible = Nothing, fvcs = Nothing}))], dpermissions = Permissions {readable = True, writable = True, executable = False, searchable = True}, daccess = Just 2020-05-06 16:06:06.1304759 UTC, dmodification = Just 2020-05-06 12:33:40.3281576 UTC}"

testingVcs :: VcsSave
testingVcs = read "VcsSave [(\"/resources/a.txt\",Vcs {vcsversions = [Version {vmessage = \"Merged revision:\\n  \\\"Merged revision:\\n  \\\"abababa\\\"\\ninto:\\n  \\\"asdsadas\\\"\\n: kept left\\\"\\ninto:\\n  \\\"Merged revision:\\n  \\\"asdsadas\\\"\\ninto:\\n  \\\"abababa\\\"\\n: kept left\\\"\\nkept both\", vcontent = \"aba \\n<<<\\ncaba\\n<<<\\n\\n>>>\\n1234\\n>>>\\n baba \\n<<<\\ndaba\\n<<<\\n\\n>>>\\n5678\\n>>>\\n eaba \\n>>>\\n9876as\\n>>>\\nf\\n>>>\\nd\\n>>>\\na\\n<<<\\nb\\n<<<\\n\\n>>>\\nsfs\\n>>>\\na\\n>>>\\nf\\n>>>\\n\"},Version {vmessage = \"Merged revision:\\n  \\\"asdsadas\\\"\\ninto:\\n  \\\"abababa\\\"\\n: kept left\", vcontent = \"aba 1234 baba 5678 eaba 9876asfdasfsaf\"},Version {vmessage = \"Merged revision:\\n  \\\"abababa\\\"\\ninto:\\n  \\\"asdsadas\\\"\\n: kept left\", vcontent = \"aba caba baba daba eaba faba\"},Version {vmessage = \"asdsadas\", vcontent = \"aba 1234 baba 5678 eaba 9876asfdasfsaf\"},Version {vmessage = \"Merged revision:\\n  \\\"abababa\\\"\\ninto:\\n  \\\"1234\\\"\\n: kept left\", vcontent = \"aba caba baba daba eaba faba\"},Version {vmessage = \"1234\", vcontent = \"aba 1234 baba 5678 eaba 9876\"},Version {vmessage = \"abababa\", vcontent = \"aba caba baba daba eaba faba\"},Version {vmessage = \"Merged revision:\\n  \\\"initial\\\"\\ninto:\\n  \\\"bob -> ab\\\"\\n: kept left\", vcontent = \"[|||o]\"},Version {vmessage = \"bob -> ab\", vcontent = \"ab\"},Version {vmessage = \"initial\", vcontent = \"[|||o]\"}]}),(\"/resources/src/Test.java\",Vcs {vcsversions = [Version {vmessage = \"initial\", vcontent = \"import java.text.DecimalFormat;\\r\\nimport java.util.Locale;\\r\\n\\r\\npublic class Test {\\r\\n    static String numberWithCommas(final double x) {\\r\\n        return new DecimalFormat(\\\"#,###.##\\\").format(x);\\r\\n    }\\r\\n    public static void main(String[] args) {\\r\\n        Locale defaul = Locale.getDefault();\\r\\n        System.out.println(numberWithCommas(1.1));\\r\\n    }\\r\\n}\\r\\n\"}]}),(\"/resources/test.test\",Vcs {vcsversions = [Version {vmessage = \"123 written instead of abacabababa\", vcontent = \"1\\n2\\n3\\n\"},Version {vmessage = \"initial\", vcontent = \"aba\\n caba\\n baba\\n\"}]})]"

spec :: Spec
spec = do
  describe "getEntity tests" $ do
    it "get a.txt" $
      getEntity testingFs "a.txt" `shouldSatisfy` isJust
    it "get something strange" $
      getEntity testingFs "abaashgafgagf" `shouldSatisfy` isNothing
    it "get bad_dir/not_writable/a" $
      getEntity testingFs "bad_dir/not_writable/a" `shouldSatisfy` isJust
  describe "list tests" $ do
    it "list small_dir files" $
      map fst (listDirFiles (fromJust $ getEntity testingFs "small_dir")) `shouldBe` ["abacaba.baba"]
    it "list all" $
      map fst (listDirFiles (fromJust $ getEntity testingFs "."))
        `shouldBe`
        [ "a.txt", "bad.txt", "not_readable.file", "not_writable.file"
        , "some_text.txt", "unaccessible.file", "abacaba.baba", "test.test"
        ]
    it "list small" $
      listDirPath testingFs "small_dir"
        `shouldBe`
        ["a", "abacaba.baba", "b"]
  describe "getSize tests" $ do
    it "small_dir size should be 14" $
      getSize (fromJust $ getEntity testingFs "small_dir") `shouldBe` 14
  describe "insert tests" $ do
    it "insert into small_dir" $
      listDirPath (fromRight undefined $ insertDir testingFs "small_dir/new_directory") "small_dir"
        `shouldBe`
        ["a", "abacaba.baba", "b", "new_directory"]
    it "insert into not_writable" $
      insertDir testingFs "bad_dir/not_writable/d" `shouldSatisfy` isLeft
    it "insert into not_writable but with the same name" $
      insertDir testingFs "bad_dir/not_writable/a" `shouldSatisfy` isRight
  describe "removeEntity tests" $ do
    it "remove a from small_dir" $
      listDirPath (fromRight undefined $ removeEntity testingFs "small_dir/a") "small_dir"
        `shouldBe`
        ["abacaba.baba", "b"]
    it "remove from not_writable" $
      removeEntity testingFs "bad_dir/not_writable/a"
        `shouldSatisfy`
        isLeft
    it "remove adsafasfas" $
      removeEntity testingFs "adsafasfas"
        `shouldSatisfy`
        isLeft
  describe "loadVcs test" $ do
    it "setVcsAll test" $ do
      let VcsSave save = testingVcs
          (withVcs, errors) = setVcsAll testingFs save
      length errors `shouldBe` 1
      (fvcs $ fromJust $ getFile withVcs "a.txt") `shouldSatisfy` isJust
      (length $ vcsversions $ fromJust $ fvcs $ fromJust $ getFile withVcs "a.txt")
        `shouldBe`
        10
  describe "vcs tests" $ do
    it "initVcs tests" $ do
      let Right (vcsInATxt, _) = initVcs testingFs "a.txt"
          aTxt = fromJust $ getFile vcsInATxt "a.txt"
          initVersion = head $ vcsversions $ fromJust $ fvcs $ aTxt
      vmessage initVersion `shouldBe` "initial"
      vcontent initVersion `shouldBe` fcontent aTxt
    it "init vcs in bad_dir" $ do
      let Right (_, errors) = initVcs testingFs "bad_dir"
      length errors `shouldBe` 2
    it "merges test: left" $ do
      let
        Right (vcsInATxt1, _) = initVcs testingFs "a.txt"
        aTxt = fromJust $ getFile vcsInATxt1 "a.txt"
        Right vcsInATxt2 = insertEntityNotRetarded vcsInATxt1 (FSFile $ aTxt { fcontent = B8.fromString $ "aba baba 5678 ba 9876asfdasfsaf lol kek"})
        Right updatedVcs = updateVcs vcsInATxt2 "inserted lol kek" "a.txt"
        Right mergedVcs = mergeVcs updatedVcs "a.txt" (getMergeStrat TakeLeft) 0 1
        aTxtAfterMerge = fromJust $ getFile mergedVcs "a.txt"
      (vcontent ((vcsversions $ fromJust $ fvcs $ aTxtAfterMerge) !! 2))
        `shouldBe`
        (vcontent ((vcsversions $ fromJust $ fvcs $ aTxtAfterMerge) !! 0))
    it "merges test: right" $ do
      let
        Right (vcsInATxt1, _) = initVcs testingFs "a.txt"
        aTxt = fromJust $ getFile vcsInATxt1 "a.txt"
        Right vcsInATxt2 = insertEntityNotRetarded vcsInATxt1 (FSFile $ aTxt { fcontent = B8.fromString $ "aba baba 5678 ba 9876asfdasfsaf lol kek"})
        Right updatedVcs = updateVcs vcsInATxt2 "inserted lol kek" "a.txt"
        Right mergedVcs = mergeVcs updatedVcs "a.txt" (getMergeStrat TakeRight) 0 1
        aTxtAfterMerge = fromJust $ getFile mergedVcs "a.txt"
      (vcontent ((vcsversions $ fromJust $ fvcs $ aTxtAfterMerge) !! 1))
        `shouldBe`
        (vcontent ((vcsversions $ fromJust $ fvcs $ aTxtAfterMerge) !! 0))
    it "merges test: both" $ do
      -- v1: "aba 1234 baba 5678 eaba 9876asfdasfsaf"
      -- v2: "aba baba 5678 ba 9876asfdasfsaf lol kek"
      let
        Right (vcsInATxt1, _) = initVcs testingFs "a.txt"
        aTxt = fromJust $ getFile vcsInATxt1 "a.txt"
        Right vcsInATxt2 = insertEntityNotRetarded vcsInATxt1 (FSFile $ aTxt { fcontent = B8.fromString $ "aba baba 5678 ba 9876asfdasfsaf lol kek"})
        Right updatedVcs = updateVcs vcsInATxt2 "inserted lol kek" "a.txt"
        Right mergedVcs = mergeVcs updatedVcs "a.txt" (getMergeStrat TakeBoth) 0 1
        aTxtAfterMerge = fromJust $ getFile mergedVcs "a.txt"
      (B8.fromString "aba <<<1234 <<<baba 5678 <<<ea<<<ba 9876asfdasfsaf>>> lol kek>>>")
        `shouldBe`
        (vcontent ((vcsversions $ fromJust $ fvcs $ aTxtAfterMerge) !! 0))


