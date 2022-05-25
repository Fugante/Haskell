module Pangram (isPangram) where

isPangram :: String -> [Bool]
isPangram [] = False
isPangram text = [letter `elem` text | letter <- ['a'..'z']]