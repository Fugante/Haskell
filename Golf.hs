{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}


-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips [1] == [[1]]
-- skips [] == []
skips :: [a] -> [[a]]
-- Inner list comprehension takes every nth element of a list.
skips l = [[x | (x, y) <- zip l [1..], y `mod` n == 0] | n <- [1..length l]]

-- Same as the function above but this uses drop instead of zip to take every nth element.
-- This version allocates less memory.
skips' :: [a] -> [[a]]
skips' l =
    [
        every n l |
        n <- [1..length l],
        let
            every m xs = case drop (m - 1) xs of
                y:ys -> y:every m ys
                [] -> []
    ]

getMax :: Int -> [Integer] -> Integer
getMax n l = maximum $ take 2 $ drop n l

localMaxima :: [Integer] -> [Integer]
localMaxima l = [getMax n l | n <- [0..length l - 3], getMax (n + 1) l == getMax n l]

addStar :: String -> String
addStar s = s ++ "*"

plotPoints :: [Integer] -> [String] -> [String]
plotPoints [] _ = []
plotPoints _ [] = []