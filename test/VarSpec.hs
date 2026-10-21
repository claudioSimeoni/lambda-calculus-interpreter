module VarSpec (spec) where

import Data.Bool (Bool (False))
import Test.Hspec
import Var

x = Var "x" 0
y = Var "y" 2
z = Var "z" 1

spec :: Spec
spec = do
  describe "showVar" $ do
    it "test 1" $ do
      showVar x `shouldBe` "x0"
      showVar y `shouldBe` "y2"
      showVar z `shouldBe` "z"
  describe "readVar" $ do
    it "test 1" $ do
      readVar "x0" `shouldBe` x
      readVar "y2" `shouldBe` y
      readVar "z" `shouldBe` z
