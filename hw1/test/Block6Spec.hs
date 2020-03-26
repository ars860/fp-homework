module Block6Spec (spec) where

import Control.Applicative ((<|>))
import Data.Char (chr, ord)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)

import Block6.Task1
import Block6.Task2
import Block6.Task3
import Block6.Task4

abcParser :: Parser Char String
abcParser = const "abc" <$> (char 'a' *> char 'b' *> char 'c')

spec :: Spec
spec = do
  describe "Task1 tests" $ do
    it "fmap id" $ do
      runP (id <$> abcParser) "abcde"
      `shouldBe`
      runP (abcParser) "abcde"
    it "fmap (f . g)" $ do
      runP (((+5) . (*5)) <$> integer) "123"
      `shouldBe`
      runP (((fmap (+5)) . (fmap (*5))) integer) "123"
    it "Applicative identity" $ do
      runP (pure id <*> integer) "123abc"
      `shouldBe`
      runP integer "123abc"
    it "Applicative homomorphism" $ do
      runP (pure (\x -> x + 1) <*> pure 0) ""
      `shouldBe`
      runP (pure $ (\x -> x + 1) 0) ""
    it "Monad second law" $ do
      runP (integer >>= return) "123abc"
      `shouldBe`
      runP integer "123abc"
    it "Monad last law" $ do
      runP
        (char 'a'
        >>= (\ch -> (\x -> char $ chr $ ord x + 1) ch
            >>= char
            )
        )
        "abb"
      `shouldBe`
      runP
        (char 'a' >>= (\x -> char $ chr $ ord x + 1) >>= char)
        "abb"
    it "Alternative test" $ do
      runP (char 'a' <|> char 'b') "abc"
        `shouldBe` Just ('a', "bc")
      runP (char 'a' <|> char 'b') "bbc"
        `shouldBe` Just ('b', "bc")
      runP (char 'a' <|> char 'b') "cbc"
        `shouldBe` Nothing
  describe "Task2 tests" $ do
    it "ok test" $ do
      runP ok "always"
        `shouldBe`
        Just ((), "always")
      runP ok (Just <$> ["works"])
        `shouldBe`
        Just ((), (Just <$> ["works"]))
    it "eof test" $ do
      runP eof "not empty"
        `shouldBe`
        Nothing
      runP eof ""
        `shouldBe`
        Just ((), "")
    it "satisfy and element test" $ do
      runP (satisfy (=='a')) "a <- it is a"
        `shouldBe`
        Just ('a', " <- it is a")
      runP (satisfy (=='a')) "b <- not a"
        `shouldBe`
        Nothing
    it "stream test" $ do
      runP (stream "abacaba") "abacabababa"
        `shouldBe`
        Just ("abacaba", "baba")
      runP (stream "abacaba") "not abacaba"
        `shouldBe`
        Nothing
  describe "Task3 test" $ do
    it "isPSP tests" $ do
      runP isPSP "()(()"
        `shouldBe`
        Nothing
      runP isPSP "(())()((()())())"
        `shouldBe`
        Just ("(())()((()())())", "")
    it "signedInteger tests" $ do
      runP signedInteger "+ - 123 not integer at all"
        `shouldBe`
        Nothing
      runP signedInteger "      +     12345definitely integer"
        `shouldBe`
        Just (12345, "definitely integer")
      runP signedInteger "      -     12345minus also works"
        `shouldBe`
        Just (-12345, "minus also works")
  describe "Task4 test" $ do
    it "listlist tests" $ do
      runP listlistParser " 3   , 1   ,  2    , 3   , 1 , 5    "
        `shouldBe`
        Just ([ [1, 2, 3], [5] ], "")
      runP listlistParser " 1   , 1   ,  2    , 3   , 1 , 1,1"
        `shouldBe`
        Just ([ [1], [3, 1], [1] ], "")
      runP listlistParser "2, 1,+10  , 3,5,-7, 2"
        `shouldBe`
        Just ([ [1, 10], [5, -7, 2] ], "")
