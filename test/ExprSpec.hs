module ExprSpec (spec) where

import Data.Bool (Bool (False))
import Expr
import Test.Hspec

e1 = Eapp (Eabs "z" (Eabs "x" (Evar (Bound 1)))) (Evar (Free "x")) -- "(\\z.\\x.z x)"
e2 = Eapp (Eabs "x" (Eapp (Evar (Bound 0)) (Evar (Bound 0)))) (Eabs "x" (Eapp (Evar (Bound 0)) (Evar (Bound 0)))) -- "(\\x.(x x) \\x.(x x))"
e3 = Eapp (Eabs "x" (Eabs "x" (Evar (Bound 0)))) (Evar (Free "y")) -- "(\\x.\\x.x y)"
e4 = Eapp (Eapp (Evar (Free "x")) (Evar (Free "x"))) (Evar (Free "x")) -- "((x x) x)"
e5 = Eapp (Eabs "x" (Eapp (Evar (Bound 0)) (Evar (Free "y")))) (Evar (Free "z")) -- "(\\x.(x y) z)"

spec :: Spec
spec = do
  describe "showExpr" $ do
    it "test 1" $ do
      showExpr e1 `shouldBe` "(\\z.\\a1.z x)"
    it "test 2" $ do
      showExpr e2 `shouldBe` "(\\x.(x x) \\x.(x x))"
    it "test 3" $ do
      showExpr e3 `shouldBe` "(\\x.\\x.x y)"
    it "test 4" $ do
      showExpr e4 `shouldBe` "((x x) x)"

  describe "readExpr" $ do
    it "test 1" $ do
      e1 `shouldBe` readExpr "(\\z.\\x.z x)"
    it "test 2" $ do
      e2 `shouldBe` readExpr "(\\x.(x x) \\x.(x x))"
    it "test 3" $ do
      e3 `shouldBe` readExpr "(\\x.\\x.x y)"
    it "test 4" $ do
      e4 `shouldBe` readExpr "((x x) x)"

  describe "reduce" $ do
    it "test 1" $ do
      reduce e1 `shouldBe` readExpr "\\y.x"
      reduce e2 `shouldBe` e2
      reduce e3 `shouldBe` readExpr "\\x.x"
      reduce e4 `shouldBe` readExpr "((x x) x)"
      reduce e5 `shouldBe` readExpr "(z y)"
      reduce e1 == readExpr "\\x.x" `shouldBe` False

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
