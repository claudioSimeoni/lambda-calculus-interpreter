module ExprParser (
    parseExpr
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Expr
import Text.TinyParser.Char
import Text.TinyParser.Core 
import Text.TinyParser.Combinators


parseExpr :: String -> Expr
parseExpr = parse expr 

expr :: Parser Expr
expr = abstraction <|> application
  where 
    abstraction = do 
        char '\\'
        v <- var
        char '.'
        e <- expr
        pure $ Eabs v e
    application = do 
        x <- atom
        fxs <- many $ do
            char ' '
            y <- atom
            pure y
        pure $ foldl (\e y -> Eapp e y) x fxs

atom :: Parser Expr
atom = parenthesized expr <|> var

var :: Parser Expr
var  = many alphanum
        