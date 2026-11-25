module Definitions where

import Expr
import Logic

zero = readExpr "\\f.\\x.x"
exprSucc = readExpr "\\n.\\f.\\x.(f ((n f) x))"
exprSum = readExpr "\\a.\\b.\\f.\\x.((b f) ((a f) x))"
exprProd = readExpr $ "\\a.\\b.(a ((" ++ show exprSum ++ ") b) (" ++ show zero ++ "))"
exprYComb = readExpr "\\f.(\\x.(f (x x))) (\\x.(f (x x)))"

exprIsZero = readExpr $ "\\n.n (\\a." ++ show false ++ ") (" ++ show true ++ ")"
exprPair = readExpr "\\a.\\b.\\s.s a b"
exprNextPair = fullReduce $ readExpr $ "\\m.(" ++ show exprPair ++ ") (m " ++ show false ++ ") ((" ++ show exprSucc ++ ") (m " ++ show false ++ "))"
zerozero = fullReduce $ readExpr $ "(" ++ show exprPair ++ ") (" ++ show zero ++ ") (" ++ show zero ++ ")"

exprPrec = fullReduce $ readExpr $ "\\n.(n (" ++ show exprNextPair ++ ") (" ++ show zerozero ++ ")) " ++ show true
exprFact =
  readExpr $
    "("
      ++ show exprYComb
      ++ ") (\\F.\\N.("
      ++ show exprIf
      ++ ") (("
      ++ show exprIsZero
      ++ ") N) ("
      ++ show (number 1)
      ++ ") (("
      ++ show exprProd
      ++ ") N "
      ++ "((F) (("
      ++ show exprPrec
      ++ ") N))))"

number :: Int -> Expr
number 0 = zero
number n = fullReduce $ Eapp exprSucc (number (n - 1))

-- | prints the output of reduction chain on separate lines
printRedChain e = putStrLn $ unwords $ map (\x -> show x ++ "\n") (reductionChain e)
