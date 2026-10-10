module ReduceSpec (spec) where

import Test.Hspec
import Expr

e1 = readExpr "\\x.\\z.x y"
e2 = readExpr "\\a.\\b.((a (\\a2.\\b2.\\f.\\x.((b2 f) ((a2 f) x)) b)) \\f.\\x.x)"

spec :: Spec
spec = describe "showExpr and readExpr" $ do
   it "reduces Expr and viceversa" $ do
    (show (reduce e1)) `shouldBe` "\\z.y0"
    (show (reduce e2)) `shouldBe` "\\a.\\b.((a \\b2.\\f.\\x.((b2 f) ((b f) x))) \\f.\\x.x)"
   
