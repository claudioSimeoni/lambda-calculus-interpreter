module Expr (
  Expr (..),
  showExpr,
  readExpr,
  reduce,
  reductionChain,
  fullReduce,
  alphaEquivalence,
) where

import Data.Set qualified as Set

data Expr = Evar Var | Eabs String Expr | Eapp Expr Expr
instance Show Expr where
  show e = showExpr e

instance Eq Expr where
  (==) = alphaEquivalence

data Var = Bound Int | Free String deriving (Eq, Ord)

--------------------------------------------------------------------------------------
-- showExpr and readExpr
--------------------------------------------------------------------------------------

-- | converts Expr to String
showExpr :: Expr -> String
showExpr e = showExprFree e [l : show c | c <- [1 ..], l <- ['a' .. 'z']] [] (freeBuild e)

-- | converts Expr to String
showExprFree :: Expr -> [String] -> [String] -> Set.Set String -> String
showExprFree (Evar (Free var)) reprList binds freeRepr = var
showExprFree (Evar (Bound var)) reprList binds freeRepr = binds !! var -- ++ show var (for debugging)
showExprFree (Eabs var expr) reprList binds freeRepr =
  "\\"
    ++ newRepr
    ++ "."
    ++ (showExprFree expr xs (newRepr : binds) freeRepr)
 where
  newReprList@(newRepr : xs)
    | not (Set.member var freeRepr) = (var : reprList)
    | otherwise = findAvailable reprList freeRepr
showExprFree (Eapp e1 e2) r@(nextRepr : xs) binds freeRepr =
  "("
    ++ (showExprFree e1 r binds freeRepr)
    ++ " "
    ++ (showExprFree e2 r binds freeRepr)
    ++ ")"

-- | it builds a set which contains all the Strings used for free variables
freeBuild :: Expr -> Set.Set String
freeBuild (Evar (Bound var)) = Set.empty
freeBuild (Evar (Free var)) = Set.singleton var
freeBuild (Eabs var expr) = freeBuild expr
freeBuild (Eapp e1 e2) = Set.union (freeBuild e1) (freeBuild e2)

{- | finds the next available String in reprList which is not already contained into freeRepr (assumes reprList is not empty)
| returns the reprList from that string on
-}
findAvailable :: [String] -> Set.Set String -> [String]
findAvailable reprList@(x : xs) freeRepr
  | (Set.member x freeRepr) = findAvailable xs freeRepr
  | otherwise = reprList

-- | converts String to Expr
readExpr :: String -> Expr
readExpr s = readExprInner s []

-- | converts String to Expr
readExprInner :: String -> [String] -> Expr
readExprInner e binds
  | length es > 1 = foldl1 Eapp (map (\x -> readExprInner x binds) es)
  | head eh == '(' = readExprInner (tail (init eh)) binds
  | head eh == '\\' =
      let (var, rest) = span (/= '.') eh
       in Eabs (tail var) (readExprInner (tail rest) ((tail var) : binds))
  | otherwise =
      if len == length binds
        then Evar (Free eh)
        else Evar (Bound len)
 where
  es@(eh : _) = splitApplication e
  len = length $ takeWhile (/= eh) binds

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

-- --------------------------------------------------------------------------------------
-- -- reduce (beta reduction)
-- --------------------------------------------------------------------------------------

-- beta reduction
reduce :: Expr -> Expr
reduce (Evar var) = Evar var
reduce (Eabs var expr) = Eabs var (reduce expr)
reduce e@(Eapp (Eabs var e1) e2) = findAndReplace e1 e2 0
reduce (Eapp e1 e2)
  | isReducible e1 = Eapp (reduce e1) e2
  | otherwise = Eapp e1 (reduce e2)

-- autoexplicative
isReducible :: Expr -> Bool
isReducible (Evar var) = False
isReducible (Eabs var expr) = isReducible expr
isReducible (Eapp (Eabs var e1) e2) = True
isReducible (Eapp e1 e2) = isReducible e1 || isReducible e2

-- finds occurences of var in e1 and substitutes with e2
findAndReplace :: Expr -> Expr -> Int -> Expr
findAndReplace (Evar (Bound v)) e2 depth
  | v == depth = addDisp e2 depth 0
  | v > depth = (Evar (Bound (v - 1)))
  | otherwise = Evar (Bound v)
findAndReplace (Eabs v1 e1) e2 depth = Eabs v1 (findAndReplace e1 e2 (depth + 1))
findAndReplace (Eapp e11 e12) e2 depth = Eapp (findAndReplace e11 e2 depth) (findAndReplace e12 e2 depth)

-- adds d to all the bind variables of e
addDisp :: Expr -> Int -> Int -> Expr
addDisp (Evar (Bound var)) disp depth = Evar (Bound (var + toAdd))
 where
  toAdd
    | var >= depth = disp
    | otherwise = 0
addDisp (Evar var) disp depth = Evar var
addDisp (Eabs var expr) disp depth = Eabs var (addDisp expr disp (depth + 1))
addDisp (Eapp e1 e2) disp depth = Eapp (addDisp e1 disp depth) (addDisp e2 disp depth)

-- --------------------------------------------------------------------------------------
-- -- complete reduction
-- --------------------------------------------------------------------------------------

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

-- --------------------------------------------------------------------------------------
-- -- alpha equivalence
-- --------------------------------------------------------------------------------------

alphaEquivalence :: Expr -> Expr -> Bool
alphaEquivalence (Evar (Bound v1)) (Evar (Bound v2)) = v1 == v2
alphaEquivalence (Evar (Free v1)) (Evar (Free v2)) = v1 == v2
alphaEquivalence (Eabs _ e1) (Eabs _ e2) = alphaEquivalence e1 e2
alphaEquivalence (Eapp e1 e2) (Eapp f1 f2) = (alphaEquivalence e1 f1) && (alphaEquivalence e2 f2)
