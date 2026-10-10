module ShowReadSpec (spec) where

import Test.Hspec
import Expr

e1 = readExpr "\\x.\\z.x y"
t1 = show e1 == "(\\x.\\z.x y)"

spec :: Spec
spec = describe "showExpr and readExpr" $ do
   it "converts a string to an Expr and viceversa" $ do
    show e1 `shouldBe` "(\\x.\\z.x y0)"
