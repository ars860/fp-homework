module Task4Spec where

import Test.Hspec ( Spec, describe, it )
import Test.Hspec.Expectations ( shouldBe )

import Task3 ( HalyavaScriptExpr (..), ArithExpr (..), Variable (..), halyavaOddPow )
import Task4 ( showExpr, showHalyavaExpr )

spec :: Spec
spec = do
  describe "Some exprs show tests" $ do
    it "simple ints expression" $ do
      let expr = AENum 1 `AEPlus` AEVar (Variable 0)
      showExpr expr `shouldBe` "(1 + var0)"
    it "simple bool expr" $ do
      let expr = AEBool True `AEAnd` AEVar (Variable 0) `AEEq` AEVar (Variable 1)
      showExpr expr `shouldBe` "((true && var0) == var1)"
    it "/|*|mod|div" $ do
      let expr = AENum 1 `AEMul` AEVar (Variable 0) `AEDiv` AENum 5 `AEMod` AEVar (Variable 1) `AEDiv` AEVar (Variable 2)
      showExpr expr `shouldBe` "Math.floor((Math.floor((1 * var0) / 5) % var1) / var2)"
    it "and some strings" $ do
      let expr = AEString "abc" `AEConcat` AEString "def" `AEConcat` AEVar (Variable 0) `AEConcat` ToString (AENum 1 `AEPlus` AENum 2)
      showExpr expr `shouldBe` "(((\"abc\" + \"def\") + var0) + (\"\" + (1 + 2)))"
  describe "halyava show tests" $ do
    it "halyavaOddPow show" $ do
      showHalyavaExpr halyavaOddPow `shouldBe` "(() => {\n  return function (var0, var1) {\n    var var2 = 0;\n    var2 = 1;\n    var var3 = \"\";\n    var3 = ((((\"\" + var0) + \" ^ \") + (\"\" + var1)) + \" = 1\");\n    while ((var1 > 0)) {\n      var1 = (var1 - 1);\n      var2 = (var2 * var0);\n      var3 = ((var3 + \" * \") + (\"\" + var0))\n    };\n    if ((var2 > 1000)) {\n      var var4 = false;\n      var4 = ((var2 % 2) == 1);\n      if (!var4) {\n        var3 = (var3 + \" |!!!| Result is not odd and greater then 1000! Fixing! |!!!|\")\n      };\n      while (!var4) {\n        var2 = Math.floor(var2 / 2);\n        var3 = (var3 + \" / 2\");\n        var4 = ((var2 % 2) == 1)\n      }\n    };\n    var3 = ((var3 + \" = \") + (\"\" + var2));\n    return var3\n  }\n})();"
    it "a + b function" $ do
      let aPlusB = Fun2 (\a b -> Return (AEVar a `AEPlus` AEVar b))
      showHalyavaExpr aPlusB `shouldBe` "(() => {\n  return function (var0, var1) {\n    return (var0 + var1)\n  }\n})();"
