import Expr

zero = readExpr "\\f.\\x.x"
exprSucc = readExpr "\\n.\\f.\\x.(f ((n f) x))"
exprSum = readExpr "\\a.\\b.\\f.\\x.((b f) ((a f) x))"
exprProd = readExpr $ "\\a.\\b.(a ((" ++ show sumo ++ ") b) (" ++ show zero ++ "))"
exprYComb = readExpr "(\\f.(\\x.(f (x x)) \\x.(f (x x)))"
exprIsZero = readExpr "\\n.n"

number :: Int -> Expr
number 0 = zero
number n = fullReduce $ Eapp successor (number (n - 1))

-- | prints the output of reduction chain on separate lines
printRedChain e = putStrLn $ unwords $ map (\x -> show x ++ "\n") (reductionChain e)
