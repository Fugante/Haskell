toDigits :: Int -> [Int]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Int -> [Int]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : y*2 : doubleEveryOther zs

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits [x]
    | x < 9 = x
    | otherwise = sumDigits (toDigits x)
sumDigits (x:xs) = sumDigits [x] + sumDigits xs

validate :: Int -> Bool
validate nums = sumDigits (doubleEveryOther (toDigitsRev nums)) `mod` 10 == 0
