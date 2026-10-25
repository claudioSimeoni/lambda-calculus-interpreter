module ExprSpec (spec) where

import Data.Bool (Bool (False))
import Expr
import Test.Hspec

spec :: Spec
spec = do
  describe "showExpr & readExpr" $ do
    it "test 1" $ do
      showExpr (readExpr "\\x.x") `shouldBe` "\\x.x"

  describe "alphaEquivalence" $ do
    it "test 1" $ do
      alphaEquivalence (readExpr "\\a.\\b.(a b)") (readExpr "\\b.\\a.(b a)") `shouldBe` True
      alphaEquivalence (readExpr "\\a.\\b.(a b)") (readExpr "\\b.\\a.(a b)") `shouldBe` False
    it "test 2" $ do
      alphaEquivalence (readExpr "a a") (readExpr "a b") `shouldBe` False
      alphaEquivalence (readExpr "a a") (readExpr "b b") `shouldBe` False
      alphaEquivalence (readExpr "\\a.b") (readExpr "\\b.b") `shouldBe` False
      alphaEquivalence (readExpr "\\a.b") (readExpr "\\a.c") `shouldBe` False
      alphaEquivalence (readExpr "\\a.b") (readExpr "\\c.b") `shouldBe` True
    it "test 3" $ do
      alphaEquivalence (readExpr "a b \\c.c") (readExpr "a b \\d.d") `shouldBe` True
      alphaEquivalence (readExpr "\\a.a a") (readExpr "\\a.(a a)") `shouldBe` False
    it "test 4" $ do
      alphaEquivalence (readExpr "\\a.(a b c \\c.(c \\a.a))") (readExpr "\\d.(d b c \\e.(e \\a.a))") `shouldBe` True
      alphaEquivalence (readExpr "\\a.(a b c \\c.(c \\a.a))") (readExpr "\\a.(d b c \\c.(c \\d.a))") `shouldBe` False
