module LogicSpec (spec) where

import Expr
import Logic
import Test.Hspec

logicTable :: Expr -> [Expr]
logicTable op = [fullReduce (Eapp (Eapp op a) b) | a <- [true, false], b <- [true, false]]

spec :: Spec
spec = do
  describe "true and false" $ do
    it "true" $ do
      fullReduce (Eapp (Eapp true (readExpr "a")) (readExpr "b"))
        `shouldBe` readExpr "a"

    it "false" $ do
      fullReduce (Eapp (Eapp false (readExpr "a")) (readExpr "b"))
        `shouldBe` readExpr "b"

  describe "logic gates + if" $ do
    it "NOT" $ do
      fullReduce (Eapp exprNot true) `shouldBe` false
      fullReduce (Eapp exprNot false) `shouldBe` true

    it "AND" $ do
      logicTable exprAnd `shouldBe` [true, false, false, false]

    it "OR" $ do
      logicTable exprOr `shouldBe` [true, true, true, false]

    it "XOR" $ do
      logicTable exprXor `shouldBe` [false, true, true, false]

    it "NAND" $ do
      logicTable exprNand `shouldBe` [false, true, true, true]

    it "NOR" $ do
      logicTable exprNor `shouldBe` [false, false, false, true]

    it "IF" $ do
      let a = readExpr "a"
          b = readExpr "b"
      fullReduce (Eapp (Eapp (Eapp exprIf true) a) b) `shouldBe` a
      fullReduce (Eapp (Eapp (Eapp exprIf false) a) b) `shouldBe` b
