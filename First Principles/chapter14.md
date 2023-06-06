# Chapter 14 exercises

## Intermission: Short Exercise

```haskell
module Main (main) where

import Test.Hspec


multiply :: (Ord a, Num a) => a -> a -> a
multiply = go 0
    where
        go :: (Ord a, Num a) => a -> a -> a -> a
        go c a b
            | a <= 0 = c
            | otherwise = go (c + b) (a - 1) b

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 times 1 is 1" $ do
            multiply 1 1 `shouldBe` 1
        it "10 times 1 is 10" $ do
            multiply 10 1 `shouldBe` 10
        it "3 times 7 is the same as 7 times 3" $ do
            multiply 3 7 `shouldBe` multiply 7 3
        it "5 times 0 is 0" $ do
            multiply 5 0 `shouldBe` 0
```


## Chapter exercises

### Validating number into words
```haskell
module WordNumberTest where

import Data.List (intersperse)
import Test.Hspec
import Test.QuickCheck

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
digits = go []
    where
        go :: [Int] -> Int -> [Int]
        go c n
            | n == 0 = c
            | otherwise = go (n `mod` 10 : c) (n `div` 10)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" . map digitToWord $ digits n


main :: IO ()
main = hspec $ do
    describe "digitToWord" $ do
        it "returns zero for 0" $ do
            digitToWord 0 `shouldBe` "zero"
        it "returns one for 1" $ do
            digitToWord 1 `shouldBe` "one"

    describe "digits" $ do
        it "returns [1] for 1" $ do
            digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $ do
            digits 100 `shouldBe` [1,0,0]

    describe "wordNumber" $ do
        it "one-zero-zero given 100" $ do
            wordNumber 100 `shouldBe` "one-zero-zero"
        it "nine-zero-zero-one for 9001" $ do
            wordNumber 9001 `shouldBe` "nine-zero-zero-one"
```

### Using QuickCheck

1.
    ```haskell
    prop_halfIdentity :: Double -> Bool
    prop_halfIdentity x = halfIdentity x == x

    runQc :: IO ()
    runQc = quickCheck prop_halfIdentity
    ```

2.
    ```haskell
    import Data.List (sort)


    -- for any list you apply sort to
    -- this property should hold
    listOrdered :: (Ord a) => [a] -> Bool
    listOrdered xs = snd $ foldr go (Nothing, True) xs
        where
            go _ status@(_, False) = status
            go y (Nothing, t) = (Just y, t)
            go y (Just x, t) = (Just y, x >= y)

    prop_listOrderedInt :: [Int] -> Bool
    prop_listOrderedInt = listOrdered . sort

    runQc :: IO ()
    runQc = quickCheck $ do
        prop_listOrderedInt
    ```

3.
    ```haskell
    plusAssociative :: (Num a, Ord a) => a -> a -> a -> Bool
    plusAssociative x y z = x + (y + z) == (x + y) + z

    plusCommutative :: (Num a, Ord a) => a -> a -> Bool
    plusCommutative x y = x + y == y + x

    prop_plusAssociativeInt :: Int -> Int -> Int -> Bool
    prop_plusAssociativeInt = plusAssociative

    prop_plusCommutativeInt :: Int -> Int -> Bool
    prop_plusCommutativeInt = plusCommutative
    ```

4.
    ```haskell
    multAssociative :: (Num a, Ord a) => a -> a -> a -> Bool
    multAssociative x y z = x * (y * z) == (x * y) * z

    multCommutative :: (Num a, Ord a) => a -> a -> Bool
    multCommutative x y = x * y == y * x

    prop_multAssociativeInt :: Int -> Int -> Int -> Bool
    prop_multAssociativeInt = multAssociative

    prop_multCommutativeInt :: Int -> Int -> Bool
    prop_multCommutativeInt = multCommutative
    ```