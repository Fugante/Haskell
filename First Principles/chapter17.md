# Chapter 17 exercises

## Exercises: Lookups

1.
    ```haskell
    added :: Maybe Integer
    added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
    ```

2.
    ```haskell
    y :: Maybe Integer
    y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

    z :: Maybe Integer
    z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

    tupled :: Maybe (Integer, Integer)
    tupled = (,) <$> y <*> z
    ```

3.
    ```haskell
    x :: Maybe Int
    x = elemIndex 3 [1, 2, 3, 4, 5]

    y :: Maybe Int
    y = elemIndex 4 [1, 2, 3, 4, 5]

    max' :: Int -> Int -> Int
    max' = max

    maxed :: Maybe Int
    maxed = max' <$> x <*> y
    ```

4.
    ```haskell
    xs = [1, 2, 3]
    ys = [4, 5, 6]

    x :: Maybe Integer
    x = lookup 3 $ zip xs ys

    y :: Maybe Integer
    y = lookup 2 $ zip xs ys

    summed :: Maybe Integer
    summed = fmap sum $ (,) <$> x <*> y
    ```


## Identity Instance

```haskell
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure :: a -> Identity a
    pure = Identity

    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    Identity f <*> Identity a = Identity $ f a
```


## Exercise: Constant Instance

```haskell
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap :: (b -> c) -> Constant a b -> Constant a c
    fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure :: Monoid a => b -> Constant a b
    pure _ = Constant mempty

    (<*>) :: Monoid a => Constant a (b -> c) -> Constant a b -> Constant a c
    (Constant a) <*> (Constant a') = Constant $ a <> a'
```


## Exercise: Fixer Upper

1.
    ```haskell
    const <$> Just "Hello" <*> pure "World"
    ```

2.
    ```haskell
    h = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
    ```


## List Applicative Exercise

```haskell
{-# LANGUAGE InstanceSigs #-}
module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
    (<>) :: List a -> List a -> List a
    l <> Nil = l
    Nil <> l' = l'
    Cons a l <> l'= Cons a (l <> l')

instance Monoid (List a) where
    mempty :: List a
    mempty = Nil

    mappend :: List a -> List a -> List a
    mappend = (<>)

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f (Cons a as) = Cons (f a) (fmap f as)
    fmap _  Nil = Nil

instance Applicative List where
    pure :: a -> List a
    pure a = Cons a Nil

    (<*>) :: List (a -> b) -> List a -> List b
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> Cons a as = Cons (f a) (f <$> as) <> (fs <*> Cons a as)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        return $ Cons a Nil

instance Eq a => EqProp (List a) where
    (=-=) = eq


x :: List (Int, Char, String)
x = Cons (1, 'c', "hola") Nil

main :: IO ()
main = quickBatch $ applicative x
```


## ZipList Applicative Exercise

```haskell
take' :: Int -> List a -> List a
take' = go 0
    where
        go :: Int -> Int -> List a -> List a
        go _ _ Nil = Nil
        go c i (Cons a ls)
            | c < i = Cons a (go (c + 1) i ls)
            | otherwise = Nil

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' =
                let (ZipList' l) = xs
                in take' 3000 l
            ys' =
                let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure a = ZipList' $ Cons a Nil
    ZipList' fs <*> ZipList' xs = ZipList' $ go fs xs
        where
            go :: List (a -> b) -> List a -> List b
            go Nil _ = Nil
            go _ Nil = Nil
            go (Cons f fs') (Cons x xs') = Cons (f x) (go fs' xs')
```

## Exercise: Variations on Either

```haskell
{-# LANGUAGE InstanceSigs #-}
module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Validation' e a = Failure' e | Success' a deriving (Eq, Show)

instance Functor (Validation' e) where
    fmap :: (a -> b) -> Validation' e a -> Validation' e b
    fmap _ (Failure' e) = Failure' e
    fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation' e) where
    pure :: a -> Validation' e a
    pure = Success'

    (<*>) :: Validation' e (a -> b) -> Validation' e a -> Validation' e b
    Failure' e <*> Failure' e'= Failure' $ e <> e'
    Failure' e <*> _ = Failure' e
    _ <*> Failure' e' = Failure' e'
    Success' f <*> Success' a = Success' $ f a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation' e a) where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        frequency [(1, return $ Failure' e), (1, return $ Success' a)]

instance (Eq e, Eq a) => EqProp (Validation' e a) where
    (=-=) = eq


x :: Validation' String (Int, Char, String)
x = Success' (1, 'c', "hola")

main :: IO ()
main = quickBatch $ applicative x
```

## Chapter Exercises

### Specialize types

1.
    ```haskell
    pure :: a -> [a]
    (<*>) :: [a -> b] -> [a] -> [b]
    ```

2.
    ```haskell
    pure:: a -> IO a
    (<*>):: IO (a -> b) -> IO a -> IO b
    ```

3.
    ```haskell
    pure :: a -> (m, a)
    (<*>) :: (m, (a -> b)) -> (m, a) -> (m, b)
    ```

4.
    ```haskell
    pure :: a -> (e -> a)
    (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
    ```

### Datatypes instances

```haskell
{-# LANGUAGE InstanceSigs #-}
module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
```

1.
    ```haskell
    data Pair a = Pair a a deriving (Eq, Show)

    instance Functor Pair where
        fmap :: (a -> b) -> Pair a -> Pair b
        fmap f (Pair a a') = Pair (f a) (f a')

    instance Applicative Pair where
        pure :: a -> Pair a
        pure a = Pair a a

        (<*>) :: Pair (a -> b) -> Pair a -> Pair b
        Pair f f' <*> Pair a a' = Pair (f a) (f' a')

    instance Arbitrary a => Arbitrary (Pair a) where
        arbitrary = do
            a <- arbitrary
            return $ Pair a a

    instance Eq a => EqProp (Pair a) where
        (=-=) = eq


    x :: Pair (Int, Char, String)
    x = Pair (1, 'k', "hola") (1, 'k', "hola")

    main :: IO ()
    main = quickBatch $ applicative x
    ```

2.
    ```haskell
    data Two a b = Two a b deriving (Eq, Show)

    instance Functor (Two m) where
        fmap :: (a -> b) -> Two m a -> Two m b
        fmap f (Two m a) = Two m (f a)

    instance Monoid m => Applicative (Two m) where
        pure :: a -> Two m a
        pure = Two mempty

        (<*>) :: Two m (a -> b) -> Two m a -> Two m b
        Two m f <*> Two m' a = Two (m <> m') (f a)

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
            a <- arbitrary
            Two a <$> arbitrary

    instance (Eq a, Eq b) => EqProp (Two a b) where
        (=-=) = eq


    x :: Two String (Int, Char, String)
    x = Two "mundo" (1, 'k', "hola")

    main :: IO ()
    main = quickBatch $ applicative x
    ```

3.
    ```haskell
    data Three a b c = Three a b c deriving (Eq, Show)

    instance Functor (Three m n) where
        fmap :: (a -> b) -> Three m n a -> Three m n b
        fmap f (Three m n a) = Three m n (f a)

    instance (Monoid m, Monoid n) => Applicative (Three m n) where
        pure :: a -> Three m n a
        pure = Three mempty mempty

        (<*>) :: Three m n (a -> b) -> Three m n a -> Three m n b
        Three m n f <*> Three m' n' a = Three (m <> m') (n <> n') (f a)

    instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            Three a b <$> arbitrary

    instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
        (=-=) = eq


    x :: Three String String (Int, Char, String)
    x = Three "hola" " mundo" (1, 'k', "!")

    main :: IO ()
    main = quickBatch $ applicative x
    ```

4.
    ```haskell
    data Three' a b = Three' a b b deriving (Eq, Show)

    instance Functor (Three' m) where
        fmap :: (a -> b) -> Three' m a -> Three' m b
        fmap f (Three' m a a') = Three' m (f a) (f a')

    instance Monoid m => Applicative (Three' m) where
        pure :: a -> Three' m a
        pure a = Three' mempty a a

        (<*>) :: Three' m (a -> b) -> Three' m a -> Three' m b
        Three' m f f' <*> Three' m' a a' = Three' (m <> m') (f a) (f' a')

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return $ Three' a b b

    instance (Eq a, Eq b) => EqProp (Three' a b) where
        (=-=) = eq


    x :: Three' String (Int, Char, String)
    x = Three' "hola" (1, 'k', "!") (1, 'k', "!")

    main :: IO ()
    main = quickBatch $ applicative x
    ```

5.
    ```haskell
    data Four a b c d = Four a b c d deriving (Eq, Show)

    instance Functor (Four m n o) where
        fmap :: (a -> b) -> Four m n o a -> Four m n o b
        fmap f (Four m n o a) = Four m n o (f a)

    instance (Monoid m, Monoid n, Monoid o) => Applicative (Four m n o) where
        pure :: a -> Four m n o a
        pure = Four mempty mempty mempty

        (<*>) :: Four m n o (a -> b) -> Four m n o a -> Four m n o b
        Four m n o f <*> Four m' n' o' a = Four (m <> m') (n <> n') (o <> o') (f a)

    instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
        Arbitrary (Four a b c d) where
            arbitrary = do
                a <- arbitrary
                b <- arbitrary
                c <- arbitrary
                Four a b c <$> arbitrary

    instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
        (=-=) = eq


    x :: Four String String String (Int, Char, String)
    x = Four "hola" "mundo" "!" (1, 'k', "!")

    main :: IO ()
    main = quickBatch $ applicative x
    ```

6.
    ```haskell
    data Four' a b = Four' a a a b deriving (Eq, Show)

    instance Functor (Four' m) where
        fmap :: (a -> b) -> Four' m a -> Four' m b
        fmap f (Four' m n o a) = Four' m n o (f a)

    instance Monoid m => Applicative (Four' m) where
        pure :: a -> Four' m a
        pure = Four' mempty mempty mempty

        (<*>) :: Four' m (a -> b) -> Four' m a -> Four' m b
        Four' m n o f <*> Four' m' n' o' a = Four' (m <> m') (n <> n') (o <> o') (f a)

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            Four' a b c <$> arbitrary

    instance (Eq a, Eq b) => EqProp (Four' a b) where
        (=-=) = eq


    x :: Four' String (Int, Char, String)
    x = Four' "hola" "mundo" "!" (1, 'k', "!")

    main :: IO ()
    main = quickBatch $ applicative x
    ```


### Combinations

```haskell
import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
```