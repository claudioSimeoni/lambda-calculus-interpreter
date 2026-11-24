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

import Data.Either (either)
import Data.List
import Data.Set qualified as Set

import Text.TinyParser.Core
import Text.TinyParser.Parsers.Lambda qualified as P

data Expr = Evar Var | Eabs String Expr | Eapp Expr Expr
instance Show Expr where
  show = showExpr

instance Eq Expr where
  (==) = alphaEquivalence

data Var = Bound Int | Free String deriving (Eq, Ord)

--------------------------------------------------------------------------------------
-- showExpr and readExpr
--------------------------------------------------------------------------------------

-- | converts Expr to String
showExpr :: Expr -> String
showExpr e = showExprInner e [l : show c | c <- [1 ..], l <- ['a' .. 'z']] [] (freeBuild e)

-- | showExpr wraps this function
showExprInner :: Expr -> [String] -> [String] -> Set.Set String -> String
showExprInner (Evar (Free var)) reprList binds freeRepr = var
showExprInner (Evar (Bound var)) reprList binds freeRepr = binds !! var -- ++ show var -- (for debugging)
showExprInner (Eabs var expr) reprList binds freeRepr =
  "\\"
    ++ newRepr
    ++ "."
    ++ showExprInner expr xs (newRepr : binds) freeRepr
 where
  (newRepr : xs)
    | not (Set.member var freeRepr) = var : reprList
    | otherwise = findAvailable reprList freeRepr
showExprInner (Eapp e1@(Eabs var e3) e2) r binds freeRepr =
  "(("
    ++ showExprInner e1 r binds freeRepr
    ++ ") "
    ++ showExprInner e2 r binds freeRepr
    ++ ")"
showExprInner (Eapp e1 e2) r binds freeRepr =
  "("
    ++ showExprInner e1 r binds freeRepr
    ++ " "
    ++ showExprInner e2 r binds freeRepr
    ++ ")"

-- | it builds a set which contains all the Strings already used for free variables
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
  | Set.member x freeRepr = findAvailable xs freeRepr
  | otherwise = reprList

-- | converts String to Expr
readExpr :: String -> Expr
readExpr s = captureVariables parsedExpr []
 where
  parsedExpr = case (runParser P.expr s) of
    Left s -> P.Evar s
    Right e -> e

  captureVariables :: P.Expr -> [String] -> Expr
  captureVariables (P.Eabs v e) binds = Eabs v (captureVariables e (v : binds))
  captureVariables (P.Eapp e1 e2) binds = Eapp (captureVariables e1 binds) (captureVariables e2 binds)
  captureVariables (P.Evar v) binds = Evar newVar
   where
    newVar = case (elemIndex v binds) of
      Just index -> Bound index
      Nothing -> Free v

--------------------------------------------------------------------------------------
-- reduce (beta reduction)
--------------------------------------------------------------------------------------

-- | beta reduction (performs the first reduction in dfs order)
reduce :: Expr -> Expr
reduce (Evar var) = Evar var
reduce (Eabs var expr) = Eabs var (reduce expr)
reduce (Eapp (Eabs var e1) e2) = findAndReplace e1 e2 0
reduce (Eapp e1 e2)
  | isReducible e1 = Eapp (reduce e1) e2
  | otherwise = Eapp e1 (reduce e2)

-- | checks if the expression is reducible
isReducible :: Expr -> Bool
isReducible (Evar var) = False
isReducible (Eabs var expr) = isReducible expr
isReducible (Eapp (Eabs var e1) e2) = True
isReducible (Eapp e1 e2) = isReducible e1 || isReducible e2

-- | finds occurences of var in e1 and substitutes with e2
findAndReplace :: Expr -> Expr -> Int -> Expr
findAndReplace (Evar (Bound v)) e2 depth
  | v == depth = addDisp e2 depth 0
  | v > depth = Evar (Bound (v - 1))
  | otherwise = Evar (Bound v)
findAndReplace (Evar (Free v)) e2 depth = Evar (Free v)
findAndReplace (Eabs v1 e1) e2 depth = Eabs v1 (findAndReplace e1 e2 (depth + 1))
findAndReplace (Eapp e11 e12) e2 depth = Eapp (findAndReplace e11 e2 depth) (findAndReplace e12 e2 depth)

-- | adds disp to all the bound variables of e which are bound to lambdas outside the expression considered
addDisp :: Expr -> Int -> Int -> Expr
addDisp (Evar (Bound var)) disp depth = Evar (Bound (var + toAdd))
 where
  toAdd
    | var >= depth = disp
    | otherwise = 0
addDisp (Evar var) disp depth = Evar var
addDisp (Eabs var expr) disp depth = Eabs var (addDisp expr disp (depth + 1))
addDisp (Eapp e1 e2) disp depth = Eapp (addDisp e1 disp depth) (addDisp e2 disp depth)

--------------------------------------------------------------------------------------
-- complete reduction
--------------------------------------------------------------------------------------

-- | returns a list with all beta reductions
reductionChain :: Expr -> [Expr]
reductionChain e
  | isReducible e = e : reductionChain (reduce e)
  | otherwise = [e]

-- | returns fully reduced expr
fullReduce :: Expr -> Expr
fullReduce e
  | isReducible e = fullReduce $ reduce e
  | otherwise = e

--------------------------------------------------------------------------------------
-- alpha equivalence
--------------------------------------------------------------------------------------

-- | checks if two expressions are alpha equivalent
alphaEquivalence :: Expr -> Expr -> Bool
alphaEquivalence (Evar (Bound v1)) (Evar (Bound v2)) = v1 == v2
alphaEquivalence (Evar (Free v1)) (Evar (Free v2)) = v1 == v2
alphaEquivalence (Eabs _ e1) (Eabs _ e2) = alphaEquivalence e1 e2
alphaEquivalence (Eapp e1 e2) (Eapp f1 f2) = alphaEquivalence e1 f1 && alphaEquivalence e2 f2
alphaEquivalence _ _ = False
