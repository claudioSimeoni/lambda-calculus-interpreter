module DefinitionsSpec (spec) where

import Definitions
import Expr
import Logic
import Test.Hspec

apply1 :: Expr -> Expr -> Expr
apply1 op a = fullReduce (Eapp op a)

apply2 :: Expr -> Expr -> Expr -> Expr
apply2 op a b = fullReduce (Eapp (Eapp op a) b)

spec :: Spec
spec = do
  describe "Church numerals" $ do
    it "number" $ do
      number 0 `shouldBe` zero
      number 1 `shouldBe` readExpr "\\f.\\x.f x"
      number 3 `shouldBe` readExpr "\\f.\\x.f (f (f x))"

    it "successor" $ do
      apply1 exprSucc zero `shouldBe` number 1
      apply1 exprSucc (number 4) `shouldBe` number 5

    it "predecessor" $ do
      apply1 exprPrec zero `shouldBe` zero
      apply1 exprPrec (number 1) `shouldBe` zero
      apply1 exprPrec (number 5) `shouldBe` number 4

  describe "Operations" $ do
    it "sum" $ do
      apply2 exprSum (number 2) (number 3) `shouldBe` number 5
      apply2 exprSum zero (number 3) `shouldBe` number 3
      apply2 exprSum (number 3) zero `shouldBe` number 3

    it "product" $ do
      apply2 exprProd (number 2) (number 3) `shouldBe` number 6
      apply2 exprProd zero (number 3) `shouldBe` zero
      apply2 exprProd (number 3) zero `shouldBe` zero
      apply2 exprProd (number 3) (number 1) `shouldBe` number 3

    it "factorial" $ do
      apply1 exprFact zero `shouldBe` number 1
      apply1 exprFact (number 1) `shouldBe` number 1
      apply1 exprFact (number 3) `shouldBe` number 6
      apply1 exprFact (number 4) `shouldBe` number 24

  describe "Other stuff" $ do
    it "isZero" $ do
      apply1 exprIsZero zero `shouldBe` true
      apply1 exprIsZero (number 1) `shouldBe` false
      apply1 exprIsZero (number 5) `shouldBe` false

    it "pair projections" $ do
      let pair = Eapp (Eapp exprPair (number 2)) (number 3)
      apply1 pair true `shouldBe` number 2
      apply1 pair false `shouldBe` number 3

    it "nextPair" $ do
      let pair = apply1 exprNextPair zerozero
      apply1 pair true `shouldBe` zero
      apply1 pair false `shouldBe` number 1
