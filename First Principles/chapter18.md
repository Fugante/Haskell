# Chatper 18 exercises

## Bind exercise

```haskell
bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ f <$> m
```

## Short Exercise:Either Monad

```haskell
{-# LANGUAGE InstanceSigs #-}

import Test.QuickCheck
import Test.QuickCheck.Checkers


data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap :: (b -> c) -> Sum a b -> Sum a c
    fmap _ (First a) = First a
    fmap f (Second b) = Second $ f b
    
instance Applicative (Sum a) where
    pure :: b -> Sum a b
    pure = Second

    (<*>) :: Sum a (b -> c) -> Sum a b -> Sum a c
    First a <*> _ = First a
    _ <*> First a' = First a'
    Second f <*> Second b = Second $ f b

instance Monad (Sum a) where
    return :: b -> Sum a b
    return = Second

    (>>=) :: Sum a b -> (b -> Sum a c) -> Sum a c
    First a >>= _ = First a
    Second b >>= f = f b

instance (Arbitary a, Arbitary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ First a, return $ Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq
```


## Chapter Exercises

### Monad instances

```haskell
{-# LANGUAGE InstanceSigs #-}
module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
```

1.
    ```haskell
    data Nope a = NopeDotJpg deriving (Eq, Show)

    instance Functor Nope where
        fmap :: (a -> b) -> Nope a -> Nope b
        fmap _ _ = NopeDotJpg

    instance Applicative Nope where
        pure :: a -> Nope a
        pure _ = NopeDotJpg

        (<*>) :: Nope (a -> b) -> Nope a -> Nope b
        _ <*> _ = NopeDotJpg

    instance Monad Nope where
        return :: a -> Nope a
        return = pure

        (>>=) :: Nope a -> (a -> Nope b) -> Nope b
        _ >>= _ = NopeDotJpg

    instance Arbitrary (Nope a) where
        arbitrary = return NopeDotJpg

    instance EqProp (Nope a) where
        (=-=) = eq


    main :: IO ()
    main = do
        let trigger :: Nope (Int, String, Int)
            trigger = undefined
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger
    ```

2.
    ```haskell
    data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

    instance Functor (PhhhbbtttEither b) where
        fmap :: (a -> c) -> PhhhbbtttEither b a -> PhhhbbtttEither b c
        fmap _ (Right' b) = Right' b
        fmap f (Left' a) = Left' $ f a

    instance Monoid b => Applicative (PhhhbbtttEither b) where
        pure :: a -> PhhhbbtttEither b a
        pure = Left'

        (<*>) :: PhhhbbtttEither b (a -> c) -> PhhhbbtttEither b a -> PhhhbbtttEither b c
        Right' b <*> _ = Right' b
        _ <*> Right' b' = Right' b'
        Left' f <*> Left' a = Left' $ f a

    instance Monoid b => Monad (PhhhbbtttEither b) where
        return :: a -> PhhhbbtttEither b a
        return = pure

        (>>=) :: PhhhbbtttEither b a -> (a -> PhhhbbtttEither b c) -> PhhhbbtttEither b c
        Right' b >>= _ = Right' b
        Left' a >>= f = f a

    instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
        arbitrary = do
            b <- arbitrary
            a <- arbitrary
            oneof [return $ Left' b, return $ Right' a]

    instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
        (=-=) = eq


    main :: IO ()
    main = do
        let trigger :: PhhhbbtttEither String (Int, String, Int)
            trigger = undefined
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger
    ```

3.
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

    instance Monad Identity where
        return :: a -> Identity a
        return = pure

        (>>=) :: Identity a -> (a -> Identity b) -> Identity b
        Identity a >>= f = f a

    instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = do Identity <$> arbitrary

    instance Eq a => EqProp (Identity a) where
        (=-=) = eq


    main :: IO ()
    main = do
        let trigger :: Identity (Int, String, Int)
            trigger = undefined
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger
    ```

4.
    ```haskell
    data List a = Nil | Cons a (List a) deriving (Eq, Show)

    instance Semigroup (List a) where
        (<>) :: List a -> List a -> List a
        l <> Nil = l
        Nil <> l' = l'
        Cons a as <> l' = Cons a (as <> l')

    instance Monoid (List a) where
        mempty :: List a
        mempty = Nil

        mappend :: List a -> List a -> List a
        mappend = (<>)

    instance Functor List where
        fmap :: (a -> b) -> List a -> List b
        fmap _ Nil = Nil
        fmap f (Cons a l) = Cons (f a) (fmap f l)

    instance Applicative List where
        pure :: a -> List a
        pure a = Cons a Nil

        (<*>) :: List (a -> b) -> List a -> List b
        Nil <*> _ = Nil
        _ <*> Nil = Nil
        Cons f fs <*> as = (f <$> as) <> (fs <*> as)

    instance Monad List where
        return :: a -> List a
        return = pure

        (>>=) :: List a -> (a -> List b) -> List b
        Nil >>= _ = Nil
        Cons a as >>= f = f a <> (as >>= f)

    instance Arbitrary a => Arbitrary (List a) where
        arbitrary = do
            a <- arbitrary
            return $ Cons a Nil

    instance Eq a => EqProp (List a) where
        (=-=) = eq


    main :: IO ()
    main = do
        let trigger :: List (Int, String, Int)
            trigger = undefined
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger
    ```

### Monad and Functor functions

1.
    ```haskell
    j :: Monad m => m (m a) -> m a
    j = join
    ```

2.
    ```haskell
    l1 :: Monad m => (a -> b) -> m a -> m b
    l1 = (<$>)
    ```

3.
    ```haskell
    l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
    l2 f a b =
    f <$> a <*> b
    ```

4.
    ```haskell
    a :: Monad m => m a -> m (a -> b) -> m b
    a = flip (<*>)
        ```

5.
    ```haskell
    meh :: Monad m => [a] -> (a -> m b) -> m [b]
    meh [] _ = return []
    meh (a:as) f = (:) <$> f a <*> meh as f
    ```

6.
    ```haskell
    flipType :: (Monad m) => [m a] -> m [a]
    flipType ms = meh ms id
    ```