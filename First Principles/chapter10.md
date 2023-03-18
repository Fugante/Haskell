# Chapter 10 exercises

## Exercises: Understanding Folds

1. `foldr(*) 1 [1..5]`

    b. `foldl (*) 1 [1..5]`

    c. `foldl (flip (*)) 1 [1..5]`

2. `foldl (flip (*)) 1 [1..3]`
    ```
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

```
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
    ```
    filterDbDate :: [DatabaseItem] -> [UTCTime]
    filterDbDate = foldr go []
        where
            go dbItem acc = case dbItem of
                DbDate time -> time : acc
                _ -> acc
    ```

2.
    ```
    filterDbNumber :: [DatabaseItem] -> [Integer]
    filterDbNumber = foldr go []
        where
            go dbItem acc = case dbItem of
                DbNumber number -> number : acc
                _ -> acc
    ```

3.
    ```
    mostRecent :: [DatabaseItem] -> UTCTime
    mostRecent = foldr go farest . filterDbDate
        where
            go dbDate closest = if dbDate > closest then dbDate else closest
            farest = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
    ```

4.
    ```
    sumDb :: [DatabaseItem] -> Integer
    sumDb = sum . filterDbNumber
    ```

5.
    ```
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

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x