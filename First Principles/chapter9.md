# Chapter 9 excercises

## Excercise: EnumFromTo

```haskell
eftBool :: Bool -> Bool -> [Bool]
eftBool b1 b2 = go b1 b2 []
    where go b0 bf c
            | bf == b0 = bf : c
            | bf < b0 = c
            | otherwise = go b0 (pred bf) (bf : c)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd ord1 ord2 = go ord1 ord2 []
    where go ord0 ordf c
            | ordf == ord0 = ordf : c
            | ordf < ord0 = c
            | otherwise = go ord0 (pred ordf) (ordf : c)

eftInt :: Int -> Int -> [Int]
eftInt n m = go n m []
    where go int0 intf c
            | intf == int0 = intf : c
            | intf < int0 = c
            | otherwise = go int0 (pred intf) (intf : c)

eftChar :: Char -> Char -> [Char]
eftChar a b = go a b []
    where go char0 charf c
            | charf == char0 = charf : c
            | charf < char0 = c
            | otherwise = go char0 (pred charf) (charf : c)
```


## Excercise: Thy Fearful Symmetry

1.
    ```haskell
    myWords :: String -> [String]
    myWords string = go string []
        where
            go text wordsList
                | text == "" = wordsList
                | otherwise = go (dropFstWord text) (wordsList ++ [takeFstWord text])
            dropFstWord text = dropWhile (==' ') $ dropWhile (/=' ') text
            takeFstWord text = takeWhile (/=' ') text
    ```

2.
    ```haskell
    myLines :: String -> [String]
    myLines string = go string []
        where
            go text linesList
                | text == "" = linesList
                | otherwise = go (dropFstLine text) (linesList ++ [takeFstLine text])
            dropFstLine text = dropWhile (=='\n') $ dropWhile (/='\n') text
            takeFstLine text = takeWhile (/='\n') text
    ```

3.
    ```haskell
    takeFst :: Char -> String -> String
    takeFst delimiter = takeWhile (/=delimiter)

    dropFst :: Char -> String -> String
    dropFst delimiter = dropWhile (==delimiter) . dropWhile (/=delimiter)

    splitText :: Char -> String -> [String]
    splitText delimiter text = go text []
        where
            go string stringsList = case string of
                "" -> stringsList
                _ -> go (dropFst delimiter string) (stringsList ++ [takeFst delimiter string])
    ```


## Excercises: Comprehend Thy Lists

```haskell
mySqr = [x^2 | x <- [1..10]]
```

1.
    ```haskell
    [x | x <- mySqr, rem x 2 == 0]
    ```

    Result:
    ```haskell
    [4, 16, 36, 64, 100]
    ```

2.
    ```haskell
    [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
    ```

    Result:
    ```haskell
    [
        (1,64), (1,81), (1,100), (4,64), (4,81), (4,100),
        (9,64), (9,81), (9,100), (16,64), (16,81), (16,100),
        (25,64), (25,81), (25,100), (36, 64), (36,81), (36,100),
        (49,64), (49,81), (49,100)
    ]
    ```

3.
    ```haskell
    take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]
    ```

    Result:
    ```haskell
    [(1,64), (1,81), (1,100), (4,64), (4,81)]
    ```


## Exercises: Square Cube

```haskell
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
```

1.
    ```haskell
    [(x, y) | x <- mySqr, y <- myCube]
    ```
2.
    ```haskell
    [(x, y) | x <- mySqr, y <- myCube, x < 50 && y < 50]
    ```
3.
    ```haskell
    length [(x, y) | x <- mySqr, y <- myCube, x < 50 && y < 50]
    ```


## Exercises: Bottom Madness

### Will it blow up?

Will the following expressions return a value or be ⊥?

1.
    ```haskell
    [x^y | x <- [1..5], y <- [2, undefined]]
    ```
    A: ⊥

2.
    ```haskell
    take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
    ```
    A: Will return `[1]`

3.
    ```haskell
    sum [1, undefined, 3]
    ```
    A: ⊥

4.
    ```haskell
    length [1, 2, undefined]
    ```
    A: Will return `3`

5.
    ```haskell
    length $ [1, 2, 3] ++ undefined
    ```
    A: ⊥
6.
    ```haskell
    take 1 $ filter even [1, 2, 3, undefined]
    ```
    A: Will return `[2]`

7.
    ```haskell
    take 1 $ filter even [1, 3, undefined]
    ```
    A: ⊥

8.
    ```haskell
    take 1 $ filter odd [1, 3, undefined]
    ```
    A: Will return `[1]`

9.
    ```haskell
    take 2 $ filter odd [1, 3, undefined]
    ```
    A: Will return `[1, 3]`

10.
    ```haskell
    take 3 $ filter odd [1, 3, undefined]
    ```
    A: ⊥


### Intermission: Is it in normal form?
0
For each expression below, determine whether it’s in:

1. normal form, which implies weak head normal form;

2. weak head normal form only; or,

3. neither.

Questions:

1.
    ```haskell
    [1, 2, 3, 4, 5]
    ```

    A: normal form

2.
    ```haskell
    1 : 2 : 3 : 4 : _[]]
    ```

    A: weak head normal form

3.
    ```haskell
    enumFromTo 1 10
    ```

    A: neither

4.
    ```haskell
    length [1, 2, 3, 4, 5]
    ```

    A: neither

5.
    ```haskell
    sum (enumFromTo 1 10)
    ```

    A: neither

6.
    ```haskell
    ['a'..'m'] ++ ['n'..'z']
    ```

    A: neither

7.
    ```haskell
    (_, 'b')
    ```

    A: weak normal form


## Exercises: More Bottoms

1.
    ```haskell
    take 1 $ map (+1) [undefined, 2, 3]
    ```

    A: ⊥

2.
    ```haskell
    take 1 $ map (+1) [1, undefined, 3]
    ```

    A: Will return `[1]`

3.
    ```haskell
    take 2 $ map (+1) [1, undefined, 3]
    ```

    A: ⊥

4.
    ```haskell
    itIsMystery xs = map (\x -> elem x "aeiou") xs
    ```

5.
    a.
    ```haskell
    map (^2) [1..10]

    [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
    ```

    b.
    ```haskell
    map minimum [[1..10], [10..20], [20..30]]
    -- n.b. `minimum` is not the same function
    -- as the `min` that we used before

    [1, 10, 20]
    ```

    c.
    ```haskell
    map sum [[1..5], [1..5], [1..5]]

    [15, 15, 15]
    ```

6.
    ```haskell
    import Data.Bool (foldBool)

    negateThree :: Real n => [n] -> [n]
    negateThree = map (\x -> bool x (-3) (x==3))
    ```


## Exercises: Filtering

1.
    ```haskell
    filter ((==0) . flip mod 3) [1..30]
    ```

2.
    ```haskell
    length $ filter ((==0) . flip mod 3) [1..30]
    ```

3.
    ```haskell
    Prelude> myFilter "the brown dog was a goof"
    ["brown","dog","was","goof"]

    filter (not . flip elem ["the", "a", "an"]) . words
    ```


## Zipping exercises

1.
    ```haskell
    zip :: [a] -> [b] -> [(a, b)]
    zip _ [] = []
    zip [] _ = []
    zip (x:xs) (y:ys) = (x, y) : myzip xs ys
    ```

2. Do what you did for zip, but now for zipWith:

    ```haskell
    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith = undefined
    ```
    ```haskell
    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith _ _ [] = []
    zipWith _ [] _ = []
    zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
    ```

3. Rewrite your zip in terms of the zipWith you wrote.
    ```haskell
    zip :: [a] -> [b] -> [(a, b)]
    zip = zipWith (,)
    ```


## Chapter Exercises

### Data.Char

1.
    ```haskell
    :t isUpper
    isUpper :: Char -> Bool

    :t toUpper
    toUpper :: Char -> Char
    ```

2.
    ```haskell
    filter isUpper
    ```

3.
    ```haskell
    capitalize :: String -> String
    capitalize text = toUpper (head text) : tail text
    ```

4.
    ```haskell
    upperCase :: String -> String
    upperCase "" = ""
    upperCase (c:cs) = toUpper c : upperCase cs
    ```

5.
    ```haskell
    :t head
    head: [a] -> a

    toUpperFst :: String -> Char
    toUpperFst string = toUpper (head string)
    ```

6.
    ```haskell
    toUpperFst :: String -> Char
    toUpperFst = toUpper . head
    ```


### Ciphers

```haskell
import Data.Char


_ASCIIa = ord 'a'
_NUMLETTERS = ord 'z' - _ASCIIa + 1

ceasar :: Char -> Int -> Char
ceasar letter shift = chr $ _ASCIIa + (ord letter + shift - _ASCIIa) `mod` _NUMLETTERS
```


### Writing your own standard functions

1.
    ```haskell
    myOr :: [Bool] -> Bool
    myOr [] = False
    myOr (x:xs) = x || myOr xs
    ```

2.
    ```haskell
    myAny :: (a -> Bool) -> [a] -> Bool
    myAny _ [] = False
    myAny predicate (x:xs) = predicate x || (myAny predicate xs)
    ```

3.
    ```haskell
    myElem :: Eq a => a -> [a] -> Bool
    myElem _ [] = False
    myElem a (x:xs) = a == x || myElem a xs

    myElem1 :: Eq a => a -> [a] -> Bool
    myElem1 a xs = any (==a) xs
    ```

4.
    ```haskell
    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse (x:xs) = myReverse xs ++ [x]
    ```

5.
    ```haskell
    squish :: [[a]] -> [a]
    squish [] = []
    squish (x:xs) = x ++ squish xs
    ```

6.
    ```haskell
    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap _ [] = []
    squishMap f (x:xs) = f x ++ squishMap f xs
    ```

7.
    ```haskell
    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap (id)
    ```

8.
    ```haskell
    myMaximumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
    myMaximumBy f l = go f l (head l)
        where
            go comparator (x:xs) max
                | xs == [] = max
                | otherwise = case comparator x max of
                    GT -> go comparator xs x
                    _ -> go comparator xs max
    ```

9.
    ```haskell
    myMinimumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
    myMinimumBy f (x:xs) = go f (x:xs) x
        where
            go comparator (x:xs) min
                | xs == [] = min
                | otherwise = case comparator x min of
                    LT -> go comparator xs x
                    _ -> go comparator xs min
    ```

10.
    ```haskell
    myMaximum :: Ord a => [a] -> a
    myMaximum = myMaximumBy compare

    myMinimum :: Ord a => [a] -> a
    myMinimum = myMinimumBy compare
    ```