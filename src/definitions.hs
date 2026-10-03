import Expr

zero = readExpr "\\f.\\x.x"
successor = readExpr "\\n.\\f.\\x.(f ((n f) x))"
sumo = readExpr "\\a.\\b.\\f.\\x.((b f) ((a f) x))"
yCombinator = readExpr "(\\f.(\\x.(f (x x)) \\x.(f (x x)))"

number :: Int -> Expr
number 0 = zero
number n = fullReduce $ Eapp successor (number (n - 1))

-- prints the output of reduction chain on separate lines
printRedChain e = putStrLn $ unwords $ map (\x -> (show x) ++ "\n") (reductionChain e)