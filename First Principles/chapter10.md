# Chapter 10 exercises

## Exercises: Understanding Folds

1. `foldr(*) 1 [1..5]`

    b. `foldl (*) 1 [1..5]`

    c. `foldl (flip (*)) 1 [1..5]`

2. `foldl (flip (*)) 1 [1..3]`
    ```haskell
    foldl (flip (*)) 1 [1..3]
    foldl(flip (*)) 1 [1,2,3]
    foldl (flip (*)) (1*1) [2,3]
    foldl (flip (*)) (1*1*(2)) [3]
    foldl (flip (*)) (1*1*(2*(3))) []
    (1*1*(2*(3)))
    (1*1*(6))
    (1*6)
    6
    ```

3. One difference between `foldr` and `foldl` is:

    c. `foldr`, but not `foldl`, associates to the right

4. Folds are catamorphisms, which means they are generally used to

    a. reduce structure

5. The following are simple folds very similar to what you’v already seen, but each has 
at least one error. Please fix them and test in your REPL:

    a. `foldr (++) ["woot", "WOOT", "woot"]`
    - Fix: `foldr (++) "" ["woot", "WOOT", "woot"]`

    b. `foldr max [] "fear is the little death"`
    - Fix: `foldr max 'a' ["fear is the little death"]`

    c. `foldr and True [False, True]`
    - Fix: `foldr (&&) True [False, True]`

    d. Can it everreturn a different answer? `foldr (||) True [False, True]`
    - `foldr (||) False [False, True]`

    e. `foldl ((++) . show) "" [1..5]`
    - Fix: `foldl (\acc x -> acc ++ show x) "" [1..5]`

    f. `foldr const 'a' [1..5]`
    - Fix: `foldl const 'a' [1..5]`

    g. `foldr const 0 "tacos"`
    - Fix: `foldl const 0 "tacos"`

    h. `foldl (flip const) 0 "burritos"`
    -Fix: `foldr (flip const) 0 "burritos"`

    i. `foldl (flip const) 'z' [1..5]`
    -Fix: `foldr (flip const) 'z' [1..5]`


## Exercises: Database Processing

```haskell
import Data.Time


data DatabaseItem =
    DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving(Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [
        DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
        DbNumber 9001,
        DbString "Hello, world!",
        DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]
```

1.
    ```haskell
    filterDbDate :: [DatabaseItem] -> [UTCTime]
    filterDbDate = foldr go []
        where
            go dbItem acc = case dbItem of
                DbDate time -> time : acc
                _ -> acc
    ```

2.
    ```haskell
    filterDbNumber :: [DatabaseItem] -> [Integer]
    filterDbNumber = foldr go []
        where
            go dbItem acc = case dbItem of
                DbNumber number -> number : acc
                _ -> acc
    ```

3.
    ```haskell
    mostRecent :: [DatabaseItem] -> UTCTime
    mostRecent = foldr go farest . filterDbDate
        where
            go dbDate closest = if dbDate > closest then dbDate else closest
            farest = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
    ```

4.
    ```haskell
    sumDb :: [DatabaseItem] -> Integer
    sumDb = sum . filterDbNumber
    ```

5.
    ```haskell
    avgDb :: [DatabaseItem] -> Double
    avgDb = avg . filterDbNumber
        where
            avg :: [Integer] -> Double
            avg xs = s / l
                where
                    s = fromIntegral $ sum xs
                    l = fromIntegral $ length xs
    ```


## Scans Exercises

```haskell
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
```

1.
    ```haskell
    fibs20 :: [Integer]
    fibs20 = take 20 $ 1 : scanl (+) 1 fibs
    ```

2.
    ```haskell
    fibsLT100 :: [Integer]
    fibsLT100 = [x | x <- fibs, x < 100]
    ```

3.
    ```haskell
    factorial :: [Integer]
    factorial = scanl (*) 1 [2..]
    ```


## Chapter Exercises

### Warm-up and Review

1.
    ```haskell
    stops  = "pbtdkg"
    vowels = "aeiou"
    ```

    a.
    ```haskell
    stopVowelStop = [(s, v, s') | s <- stops, v <- vowels, s' <- stops]
    ```

    b.
    ```haskell
    stopVowelsStop' = [('p', v, s') | v <- vowels, s' <- stops]
    ```

    c.
    ```haskell
    nounVerbNoun :: [String] -> [String] -> [String]
    nounVerbNoun nouns verbs = [n ++ v ++ n' | n <- nouns, v <- verbs, n' <- nouns]
    ```

2.
    ```haskell
    seekritFunc x = div (sum (map length (words x))) (length (words x))
    ```
    Gives the ratio of letters to words in a sentence.

3.
    ```haskell
    seekritFunc' :: Fractional a => String -> a
    seekritFunc' x = y / z
        where y = fromIntegral $ sum $ map length $ words x
            z = fromIntegral $ length $ words x
    ```

### Rewriting functions using folds

1.
    ```haskell
    myOr :: [Bool] -> Bool
    myOr = foldr (||) False
    ```

2.
    ```haskell
    myAny :: (a -> Bool) -> [a] -> Bool
    myAny f = myOr . map f
    ```

3.
    ```haskell
    myElem :: Eq a => a -> [a] -> Bool
    myElem a = foldr go False
        where
            go x acc = acc || a == x

    myElem' :: Eq a => a -> [a] -> Bool
    myElem' a = any (a==)
    ```

4.
    ```haskell
    myReverse :: [a] -> [a]
    myReverse = foldr (\a acc -> acc ++ [a]) []
    ```

5.
    ```haskell
    myMap :: (a -> b) -> [a] -> [b]
    myMap f = foldr (\a acc -> f a : acc) []
    ```

6.
    ```haskell
    myFilter :: (a -> Bool) -> [a] -> [a]
    myFilter p = foldr go []
        where
            go a acc
                | p a = a : acc
                | otherwise = acc
    ```

7.
    ```haskell
    squish :: [[a]] -> [a]
    squish = foldr (++) []
    ```

8.
    ```haskell
    squishMap :: (a -> [b]) -> [a] -> [b] 
    squishMap f = foldr (\a acc -> f a ++ acc) []
    ```

9.
    ```haskell
    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap id
    ```

10.
    ```haskell
    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy f (x:xs) = foldl go x (x:xs)
        where
            go acc a = case f acc a of
                GT -> acc
                _ -> a
    ```

11.
    ```haskell
    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
    myMinimumBy f (x:xs) = foldl go x (x:xs)
        where
            go acc a = case f acc a of
                LT -> acc
                _ -> a
    ```