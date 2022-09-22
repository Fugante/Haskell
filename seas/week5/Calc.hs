module Calc where

import ExprT
import Parser (parseExp)


eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    Just e -> Just (eval e)
    Nothing -> Nothing