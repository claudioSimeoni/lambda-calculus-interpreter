module Var (
  Var (..),
  showVar,
  readVar,
) where

import Data.Char (isDigit)

data Var = Var String Int deriving (Eq, Ord)
instance Show Var where
  show = showVar

-- converts from var to string
showVar :: Var -> [Char]
showVar (Var var num) = var ++ id
 where
  id =
    if num == 1
      then ""
      else show num

-- converts from string to var, takes as a number everything starting from the first digit it finds
-- TODO: better error handling (something like x0a0 will crash)
readVar :: String -> Var
readVar s = Var str n
 where
  (str, num) = span (not . isDigit) s
  n =
    if num == ""
      then 1
      else read num :: Int
