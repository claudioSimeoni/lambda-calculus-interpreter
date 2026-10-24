module Expr (
  Expr (..),
  showExpr,
  -- readExpr,
  -- reduce,
  -- reductionChain,
  -- fullReduce,
  -- alphaEquivalence,
  Var (..),
  -- readVar,
) where

import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Var

data Expr = Evar Var | Eabs String Expr | Eapp Expr Expr
instance Show Expr where
  show e = showExpr e

-- instance Eq Expr where
--   (==) = alphaEquivalence

--------------------------------------------------------------------------------------
-- showExpr and readExpr
--------------------------------------------------------------------------------------

-- | converts Expr to String
showExpr :: Expr -> String
showExpr e = showExprFree e [l : show c | c <- [1 ..], l <- ['a' .. 'z']] [] (freeBuild e)

-- | converts Expr to String
showExprFree :: Expr -> [String] -> [String] -> Set.Set String -> String
showExprFree (Evar (Free var)) reprList binds freeRepr = var
showExprFree (Evar (Bound var)) reprList binds freeRepr = binds !! var
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

-- beta reduction (a wrapper of reduceLabeled: labels an expression before reducing it)
reduce :: Expr -> Expr
reduce e = reduceLabeled (label e Map.empty Map.empty)

-- beta reduction of a labeled expression
reduce :: Expr -> Expr
reduce (Evar var) = Evar var
reduce (Eabs var expr) = Eabs var (reduce expr)
reduce e@(Eapp (Eabs var e1) e2) = findAndReplace e1 e2 var
reduce (Eapp e1 e2)
  | isReducible e1 = Eapp (reduce e1) e2
  | otherwise = Eapp e1 (reduce e2)

-- beta reduction utility
reduce :: Expr -> Int -> Expr
reduce (Evar var) = Evar var
reduce (Eabs var expr) = Eabs var (reduce expr)
reduce e@(Eapp (Eabs var e1) e2) = findAndReplace e1 e2 var
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
findAndReplace :: Expr -> Expr -> Var -> Expr
findAndReplace (Evar v1) e2 var
  | v1 == var = e2
  | otherwise = Evar v1
findAndReplace (Eabs v1 e1) e2 var = Eabs v1 (findAndReplace e1 e2 var)
findAndReplace (Eapp e11 e12) e2 var = Eapp (findAndReplace e11 e2 var) (findAndReplace e12 e2 var)

-- assigns a label to each variable in an Expr
label :: Expr -> Map.Map Var Int -> Map.Map String Int -> Expr
label (Evar var@(Var v num)) mv ms = Evar (Var v val)
 where
  l = Map.lookup var mv
  val = extrMaybeInt l
label (Eabs var@(Var v num) e) mv ms = Eabs (Var v val) (label e newmv newms)
 where
  newms = (Map.insertWith (+) v 1 ms)
  l = Map.lookup v newms
  val = extrMaybeInt l
  newmv = (Map.insert var val mv)
label (Eapp e1 e2) mv ms = Eapp (label e1 mv ms) (label e2 mv ms)

-- extracts a value from a Maybe object
extrMaybeInt :: Maybe Int -> Int
extrMaybeInt Nothing = 0
extrMaybeInt (Just val) = val

-- --------------------------------------------------------------------------------------
-- -- complete reduction
-- --------------------------------------------------------------------------------------

-- -- returns a list with all beta reductions
-- reductionChain :: Expr -> [Expr]
-- reductionChain e
--   | isReducible e = e : reductionChain (reduce e)
--   | otherwise = [e]

-- -- returns fully reduced expr
-- fullReduce :: Expr -> Expr
-- fullReduce e
--   | isReducible e = fullReduce $ reduce e
--   | otherwise = e

-- --------------------------------------------------------------------------------------
-- -- alpha equivalence
-- --------------------------------------------------------------------------------------

-- alphaEquivalence :: Expr -> Expr -> Bool
-- alphaEquivalence e1 e2 = aEq (label e1 Map.empty Map.empty) (label e2 Map.empty Map.empty) Map.empty Map.empty

-- aEq :: Expr -> Expr -> Map.Map Var Var -> Map.Map Var Var -> Bool
-- aEq (Evar v1) (Evar v2) map1 map2 = varEq
--  where
--   v1T = Map.lookup v1 map1
--   v2T = Map.lookup v2 map2
--   varEq =
--     if isNothing v1T && isNothing v1T
--       then v1 == v2
--       else v1T == Just v2 && v2T == Just v1
-- aEq (Eabs v1 e1) (Eabs v2 e2) map1 map2
--   | varEq = aEq e1 e2 updMap1 updMap2
--   | otherwise = False
--  where
--   updMap1 = Map.insertWith (\a b -> b) v1 v2 map1
--   updMap2 = Map.insertWith (\a b -> b) v2 v1 map2
--   v1T = Map.lookup v1 updMap1
--   v2T = Map.lookup v2 updMap2
--   varEq = v1T == Just v2 && v2T == Just v1
-- aEq (Eapp e11 e12) (Eapp e21 e22) map1 map2 = eqLeft && eqRight
--  where
--   eqLeft = aEq e11 e21 map1 map2
--   eqRight = aEq e12 e22 map1 map2
-- aEq _ _ map1 map2 = False
