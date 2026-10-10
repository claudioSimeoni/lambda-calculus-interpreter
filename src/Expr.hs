module Expr (
    Expr (..),
    Var (..),
    showExpr,
    readExpr,
    reduce,
    reductionChain,
    fullReduce,
    alphaEquivalence,
) where

import qualified Data.Map as Map

data Var = Var String Int deriving (Eq, Ord)
instance Show Var where
    show (Var var num) = var ++ id
      where
        id =
            if num == 1
                then ""
                else show num

data Expr = Evar Var | Eabs Var Expr | Eapp Expr Expr
instance Show Expr where
    show e = showExpr (label e Map.empty)

instance Eq Expr where
    (==) = alphaEquivalence

--------------------------------------------------------------------------------------
-- showExpr and readExpr
--------------------------------------------------------------------------------------

-- | converts Expr to String
showExpr :: Expr -> String
showExpr (Evar var) = show var
showExpr (Eabs var expr) = "\\" ++ show var ++ "." ++ showExpr expr
showExpr (Eapp e1 e2) = "(" ++ showExpr e1 ++ " " ++ showExpr e2 ++ ")"

-- | converts String to Expr
readExpr :: String -> Expr
readExpr e
    | length es > 1 = foldl1 Eapp (map readExpr es)
    | head eh == '(' = readExpr $ tail $ init eh
    | head eh == '\\' =
        let (var, rest) = span (/= '.') eh
         in Eabs (Var (tail var) 1) (readExpr (tail rest))
    | otherwise = Evar (Var eh 1)
  where
    es@(eh : _) = splitApplication e

-- takes a string and splits at all function application points
splitApplication :: String -> [String]
splitApplication [] = []
splitApplication (' ' : xs) = splitApplication xs
splitApplication x = first : splitApplication rest
  where
    prefPar (acc, _) c =
        ( acc + case c of
            '(' -> 1
            ')' -> -1
            c -> 0
        , c
        )
    len = length $ takeWhile (/= (0, ' ')) (scanl prefPar (0, '#') x)
    (first, rest) = splitAt (len - 1) x

--------------------------------------------------------------------------------------
-- reduce (beta reduction)
--------------------------------------------------------------------------------------

-- beta reduction (a wrapper of reduceLabeled: labels an expression before reducing it)
reduce :: Expr -> Expr
reduce e = reduceLabeled (label e Map.empty)

-- beta reduction of a labeled expression
reduceLabeled :: Expr -> Expr
reduceLabeled (Evar var) = Evar var
reduceLabeled (Eabs var expr) = Eabs var (reduceLabeled expr)
reduceLabeled e@(Eapp (Eabs var e1) e2) = findAndReplace e1 e2 var
reduceLabeled (Eapp e1 e2)
    | isReducible e1 = Eapp (reduceLabeled e1) e2
    | otherwise = Eapp e1 (reduceLabeled e2)

-- autoexplicative
isReducible :: Expr -> Bool
isReducible (Evar var) = False
isReducible (Eabs var expr) = isReducible expr
isReducible (Eapp (Eabs var e1) e2) = True
isReducible (Eapp e1 e2) = isReducible e1 || isReducible e2

-- finds occurences of var in e1 and substitutes with e2
findAndReplace :: Expr -> Expr -> Var -> Expr
findAndReplace (Evar v1) e2 var
    | v1 == var = e2
    | otherwise = Evar v1
findAndReplace (Eabs v1 e1) e2 var = Eabs v1 (findAndReplace e1 e2 var)
findAndReplace (Eapp e11 e12) e2 var = Eapp (findAndReplace e11 e2 var) (findAndReplace e12 e2 var)

-- assigns a label to each variable in an Expr
label :: Expr -> Map.Map Var Int -> Expr
label (Evar var@(Var v num)) m = Evar (Var v val)
  where
    l = Map.lookup var m
    val = extrMaybeInt l
label (Eabs var@(Var v num) e) m = Eabs (Var v val) (label e newm)
  where
    newm = Map.insertWith (+) var 1 m
    l = Map.lookup var newm
    val = extrMaybeInt l
label (Eapp e1 e2) m = Eapp (label e1 m) (label e2 m)

-- extracts a value from a Maybe object
extrMaybeInt :: Maybe Int -> Int
extrMaybeInt Nothing = 0
extrMaybeInt (Just val) = val

--------------------------------------------------------------------------------------
-- complete reduction
--------------------------------------------------------------------------------------

-- returns a list with all beta reductions
reductionChain :: Expr -> [Expr]
reductionChain e
    | isReducible e = e : reductionChain (reduce e)
    | otherwise = [e]

-- returns fully reduced expr
fullReduce :: Expr -> Expr
fullReduce e
    | isReducible e = fullReduce $ reduce e
    | otherwise = e

--------------------------------------------------------------------------------------
-- alpha equivalence
--------------------------------------------------------------------------------------

alphaEquivalence :: Expr -> Expr -> Bool
alphaEquivalence e1 e2 = aEq (label e1 Map.empty) (label e2 Map.empty) Map.empty Map.empty

aEq :: Expr -> Expr -> Map.Map Var Var -> Map.Map Var Var -> Bool
aEq (Evar v1) (Evar v2) map1 map2 = varEq
    where updMap1 = Map.insertWith (\a b -> b) v1 v2 map1
          updMap2 = Map.insertWith (\a b -> b) v2 v1 map2
          v1T = Map.lookup v1 updMap1
          v2T = Map.lookup v2 updMap2
          varEq = v1T == Just v2 && v2T == Just v1

aEq (Eabs v1 e1) (Eabs v2 e2) map1 map2
    | varEq     = aEq e1 e2 updMap1 updMap2
    | otherwise = False 
    where updMap1 = Map.insertWith (\a b -> b) v1 v2 map1
          updMap2 = Map.insertWith (\a b -> b) v2 v1 map2
          v1T = Map.lookup v1 updMap1
          v2T = Map.lookup v2 updMap2
          varEq = v1T == Just v2 && v2T == Just v1

aEq (Eapp e11 e12) (Eapp e21 e22) map1 map2 = eqLeft && eqRight
    where eqLeft  = aEq e11 e21 map1 map2
          eqRight = aEq e12 e22 map1 map2

aEq _ _ map1 map2 = False
