{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}

import Data.List
import Data.Char


-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips [1] == [[1]]
-- skips [] == []
skips :: [a] -> [[a]]
-- Inner list comprehension takes every nth element of a list.
skips l = [[x | (x, y) <- zip l [1..], y `mod` n == 0] | n <- [1..length l]]

-- Same as the function above but this uses drop instead of zip to take every nth element.
-- This version allocates less memory and is also faster.
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

-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima l = [getMax n l | n <- [0..length l - 3], getMax (n + 1) l == getMax n l]


{- Count the ocurrences of a given number -}
countNum :: Integer -> [Integer] -> Int
countNum _ [] = 0
countNum n nums = length $ filter (==n) nums

{- Count the ocurrences of the given numbers to create an array of frequencies -}
numsFreq :: [Integer] -> [Integer] -> [Int]
numsFreq [] _ = []
numsFreq count nums = [countNum n nums | n <- count]

{-
Transform an array of frequencies into an array of repeating characters. Each string is 
as long as the number it represents.
-}
freqsToChar :: [Int] -> Char -> [String]
freqsToChar freq c = [replicate f c | f <- freq]

{- Create a bar plot -}
plot :: [String] -> Int -> String
plot bars size =
    concat [
        -- Add a new-line character to every row
        r ++ "\n" |
        -- Add padding (blank space) to the lines -> transform rows to columns ->
        -- Reverse the rows (plot is upside down)
        r <- reverse $ transpose [b ++ replicate (size - length b) ' ' | b <- bars]
    ]

-- Define the constants required for plotting a histogram
low :: Int; up :: Int
low' :: Integer; up' :: Integer
(low, up) = (1, 9)
(low', up') = (1, 9)

-- histogram [1,1,1,5] ==
-- *
-- *
-- * *
-- ==========
-- 0123456789
histogram :: [Integer] -> String
histogram nums =
    -- Create plot
    plot (freqsToChar (numsFreq [low'..up'] nums) '*') up
    -- Add a line of "=" characters
    ++ replicate up '=' ++ "\n"
    -- Add a line with all the numbers being counted
    ++ [intToDigit $ fromIntegral n | n <- [low'..up']] ++ "\n"
