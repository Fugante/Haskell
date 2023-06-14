# Chapter 20 exercises

## Exercises: Library Functions

```haskell
import Data.Monoid
    (  Sum      (Sum, getSum)
     , Product  (Product, getProduct)
    )
```

1.
```haskell
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum
```

2.
    ```haskell
    product :: (Foldable t, Num a) => t a -> a
    product = getProduct . foldMap Product
    ```

3.
    ```haskell
    elem :: (Foldable t, Eq a) => a -> t a -> Bool
    elem a = foldr isElem False
        where
            isElem a' acc = a' == a || acc
    ```

4.
    ```haskell
    minimum :: (Foldable t, Ord a) => t a -> Maybe a
    minimum = foldr isLower Nothing
        where
            isLower c Nothing = Just c
            isLower c (Just acc)
                | c < acc = Just c
                | otherwise = Just acc
    ```

5.
    ```haskell
    maximum :: (Foldable t, Ord a) => t a -> Maybe a
    maximum = foldr isHigher Nothing
        where
            isHigher c Nothing = Just c
            isHigher c (Just acc)
                | c > acc = Just c
                | otherwise = Just acc
    ```

6.
    ```haskell
    null :: (Foldable t) => t a -> Bool
    null = foldr any' True
        where
            any' :: a -> Bool -> Bool
            any' a _ = False
    ```

7.
    ```haskell
    length :: (Foldable t) => t a -> Int
    length = foldr count 0
        where
            count :: a -> Int -> Int
            count _ acc = acc + 1
    ```

8.
    ```haskell
    toList :: (Foldable t) => t a -> [a]
    toList = foldr append []
        where
            append :: a -> [a] -> [a]
            append a acc = a : acc
    ```

9.
    ```haskell
    fold :: (Foldable t, Monoid m) => t m -> m
    fold = foldMap id
    ```

10.
    ```haskell
    foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
    foldMap f = foldr f' mempty
        where
            f' a acc = f a <> acc
    ```


### Chapter Exercises

```haskell
{-# LANGUAGE InstanceSigs #-}
```

1.
```haskell
data Constant a b = Constant b

instance Foldable (Constant a) where
    foldr :: (b -> c -> c) -> c -> Constant a b -> c
    foldr f c (Constant b) = f b c
```

2.
```haskell
data Two a b = Two a b

instance Foldable (Two a) where
    foldr :: (b -> c -> c) -> c -> Two a b -> c
    foldr f c (Two _ b) = f b c
```

3.
```haskell
data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldr :: (c -> d -> d) -> d -> Three a b c -> d
    foldr f d (Three _ _ c) = f c d
```

4.
```haskell
data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldMap :: Monoid m => (b -> m) -> Three' a b -> m
    foldMap f (Three' _ b b') = f b <> f b'
```

5.
```haskell
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldMap :: Monoid m => (b -> m) -> Four' a b -> m
    foldMap f (Four' _ b0 b1 b2) = f b0 <> f b1 <> f b2
```


```haskell
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p = foldMap appendIf
    where
        appendIf a = if p a then pure a else mempty
```