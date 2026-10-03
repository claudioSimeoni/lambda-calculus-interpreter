module Expr (
    Expr(..),
    showExpr,
    readExpr,
    reduce,
    reductionChain,
    fullReduce
) where

data Expr = Evar String | Eabs String Expr | Eapp Expr Expr

instance Show Expr where
    show = showExpr
   
-- converts Expr to String
showExpr :: Expr -> String
showExpr (Evar var) = var
showExpr (Eabs var expr) = "\\" ++ var ++ "." ++ showExpr expr
showExpr (Eapp e1 e2) = "(" ++ showExpr e1 ++ " " ++ showExpr e2 ++ ")"

-- converts String to Expr
readExpr :: String -> Expr
readExpr ('\\' : e) = Eabs (take len e) (readExpr (drop (len + 1) e))
    where len = length $ takeWhile (\c -> c /= '.') e
readExpr ('(' : e) = 
    Eapp (readExpr (take (len - 1) e)) (readExpr (init (drop len e)))
    where prefPar (accs, _) c = 
            (accs + case c of 
                        '(' -> 1
                        ')' -> -1
                        c -> 0, 
            c)
          len = length $ takeWhile (\(s, c) -> s /= 0 || c /= ' ') (scanl prefPar (0, '#') e)
readExpr s = Evar s

-- autoexplicative
isReducible :: Expr -> Bool
isReducible (Evar var) = False
isReducible (Eabs var expr) = isReducible expr
isReducible (Eapp (Eabs var e1) e2) = True
isReducible (Eapp e1 e2) = isReducible e1 || isReducible e2

-- finds occurences of v in e1 and substitutes with e2
findAndReplace :: Expr -> Expr -> String -> Expr
findAndReplace (Evar v1) e2 var
    | v1 == var = e2
    | otherwise = Evar v1
findAndReplace (Eabs v1 e1) e2 var = Eabs v1 (findAndReplace e1 e2 var)
findAndReplace (Eapp e11 e12) e2 var = Eapp (findAndReplace e11 e2 var ) (findAndReplace e12 e2 var)


-- beta reduction
reduce :: Expr -> Expr
reduce (Evar var) = Evar var
reduce (Eabs var expr) = Eabs var (reduce expr)
reduce (Eapp (Eabs var e1) e2) = findAndReplace e1 e2 var
reduce (Eapp e1 e2)
    | isReducible e1 = Eapp (reduce e1) e2
    | otherwise      = Eapp e1 (reduce e2)


-- returns a list with all beta reductions
reductionChain :: Expr -> [Expr]
reductionChain e
    | isReducible e = e : reductionChain (reduce e)
    | otherwise     = [e]

-- returns fully reduced expr
fullReduce :: Expr -> Expr
fullReduce e
    | isReducible e = fullReduce $ reduce e
    | otherwise     = e
