# Chapter 8 excercises

## Intermission: Excersice

```haskell
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n - 1) f b)

applyTimes' 0 f b = b
applyTimes' n f b = f . applyTimes (n - 1) f $ b

{-
    applyTimes 5 (+1) 5
    ==> (+1) (applyTimes 4 (+1) 5)
    ==> (+1) ((+1) (applyTimes 3 (+1) 5)) 
    ==> (+1) ((+1) ((+1) applyTimes 2 (+1) 5))
    ==> (+1) ((+1) ((+1) ((+1) applyTimes 1 (+1) 5)))
    ==> (+1) ((+1) ((+1) ((+1) ((+1) applyTimes 0 (+1) 5)()))
    ==> (+1) ((+1) ((+1) ((+1) ((+1) 5))))
    ==> (+1) ((+1) ((+1) ((+1) 6)))
    ==> (+1) ((+1) ((+1) 7))
    ==> (+1) ((+1) 8)
    ==> (+1) 9
    ==> 10
-}
```

## Chapter excersices

### Review of types

1. What is the type of `[[True, False], [True, True], [False, True]]`?

    d) `[[Bool]]`

2. Which of the following has the same type as `[[True, False], [True, True], [False, True]]`?

    b) `[[3 == 3], [5 > 6], [3 < 4]]`

3. For the following function

        func :: [a] -> [a] -> [a]
        func x y = x ++ y
    which of the following is true?

    d) all of the above

4. For the `func`code above, which is a valid application of `func` to both of its arguments?

    b) `func "Hello" "World"`


### Reviewing currying

```haskell
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types


flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
```

1. `"woops mrow woohoo!"`

2. `"1 mrow haha"`

3. `"woops mrow 2 mrow haha"`

4. `"woops mrow blue mrow haha"`

5. `"pink mrow haha mrow green mrow woops mrow blue"`

6. `"are mrow Pugs mrow awesome"`


### Recursion

1.
    ```haskell
    dividedBy :: Integral a => a -> a -> (a, a)
    dividedBy num denom = go num denom 0
        where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)
    {-
        dividedBy 15 2
        ==> go 15 2 0
        ==> go 13 2 1
        ==> go 11 2 3
        ==> go 9 2 4
        ==> go 7 2 5
        ==> go 5 2 6
        ==> go 3 2 7
        ==> go 1 2 8
        ==> (8, 1)
    -}
    ```

2.
    ```haskell
    summatory :: (Eq a, Num a) a => a -> a
    summatory num = go num 0
        where go n c
                | n == 0 = c
                | otherwise = go (n - 1) (c + n)
    ```

3.
    ```haskell
    recMult :: Integral a => a -> a -> a
    recMult m n = go n m 0
    where go a b c
            | b <= 0 = c
            | otherwise = go a (b - 1) (c + a)
    ```


### Fixing dividedBy

```haskell
data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = Result count
            | otherwise = go (n - d) d (count + 1)
```


### McCarthy 91 function

```haskell
mc91 :: (Num a, Ord a) => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 $ mc91 $ n + 11
```


### Numbers into words

```haskell
module WordNumber where

import Data.List (intersperse)


digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n = go n []
    where go num c
            | num == 0 = c
            | otherwise = go (num `div` 10) (num `mod` 10 : c)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" . map digitToWord $ digits n
```