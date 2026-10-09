module Logic (
    true,
    false,
    exprNot,
    exprAnd,
    exprOr,
    exprXor,
    exprNand,
    exprNor
) where

import Expr

-- definitions of true and false
true =  readExpr "\\x.\\y.x"
false = readExpr "\\x.\\y.y"

-- basic logic gates
exprNot = readExpr $ "\\a.(a " ++ show false ++ " " ++ show true ++ ")"
exprAnd = readExpr $ "\\a.\\b.(a b " ++ show false ++ ")"
exprOr  = readExpr $ "\\a.\\b.(a " ++ show true ++ " b)"
exprXor = readExpr $ "\\a.\\b.(a " ++ show (Eapp exprNot (readExpr "b")) ++ " b)"

-- derive the other logic gates (surely this can be done better)
exprNand = readExpr $ "\\a.\\b.(" ++ show (Eapp exprNot (Eapp (Eapp exprAnd (readExpr "a")) (readExpr "b"))) ++ ")"
exprNor = readExpr $ "\\a.\\b.(" ++ show (Eapp exprNot (Eapp (Eapp exprOr (readExpr "a")) (readExpr "b"))) ++ ")"

-- useful for testing
printLogicTable2 :: Expr -> [Expr]
printLogicTable2 op = [fullReduce $ Eapp (Eapp op a) b | a <- [true, false], b <- [true, false]]
