module ExprSpec (spec) where

import Data.Bool (Bool (False))
import Expr
import Expr (alphaEquivalence, readExpr)
import Test.Hspec

x = Var "x" 0
y = Var "y" 2
z = Var "z" 1

-- \x.\z.x y
e1 = Eapp (Eabs x (Eabs z (Evar x))) (Evar y)
e2 = Eapp (Eapp (Evar x) (Evar x)) (Evar x)

spec :: Spec
spec = do
  describe "showExpr" $ do
    it "test 1" $ do
      showExpr e1 `shouldBe` "(\\x0.\\z.x0 y2)"
    it "test 2" $ do
      showExpr e2 `shouldBe` "((x0 x0) x0)"

  describe "readExpr" $ do
    it "test 1" $ do
      readExpr "(\\x.\\z.x y0)" `shouldBe` e1
    it "test 2" $ do
      readExpr "x0 x0 x0" `shouldBe` e2

  describe "alphaEquivalence" $ do
    it "test 1" $ do
      alphaEquivalence (readExpr "\\a.\\b.(a b)") (readExpr "\\b.\\a.(b a)") `shouldBe` True
      alphaEquivalence (readExpr "\\a.\\b.(a b)") (readExpr "\\b.\\a.(a b)") `shouldBe` False
    it "test 2" $ do
      alphaEquivalence (readExpr "a a") (readExpr "a b") `shouldBe` False
      alphaEquivalence (readExpr "\\a.b") (readExpr "\\b.b") `shouldBe` False
      alphaEquivalence (readExpr "\\a.b") (readExpr "\\a.c") `shouldBe` False
      alphaEquivalence (readExpr "\\a.b") (readExpr "\\c.b") `shouldBe` True
    it "test 3" $ do
      alphaEquivalence (readExpr "a b \\c.c") (readExpr "a b \\d.d") `shouldBe` True
      alphaEquivalence (readExpr "\\a.a a") (readExpr "\\a.(a a)") `shouldBe` False
    it "test 4" $ do
      alphaEquivalence (readExpr "\\a.(a b c \\c.(c \\a.a))") (readExpr "\\d.(d b c \\e.(e \\a.a))") `shouldBe` True
      alphaEquivalence (readExpr "\\a.(a b c \\c.(c \\a.a))") (readExpr "\\a.(d b c \\c.(c \\d.a))") `shouldBe` False
