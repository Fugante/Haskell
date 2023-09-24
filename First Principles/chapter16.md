# Chapter 16 exercises

## Exercises: Be Kind

1. `*`

2. The kind of `b` is `* -> *`. The kind of `T` is `* -> *`

3. `* -> * -> *`


## Exercises: Heavy Lifting

1.
    ```haskell
    a :: [Int]
    a = fmap (+1) $ read "[1]" :: [Int]
    ```

2.
    ```haskell
    b :: Maybe [[Char]]
    b = (fmap . fmap) (++ "lol") $ Just ["Hi,", "Hello"]
    ```

3.
    ```haskell
    c :: Int -> Int
    c = (*2) . (\x -> x - 2)
    ```

4.
    ```haskell
    d :: Int -> [Char]
    d = ((return '1' ++) . show) . (\x -> [x, 1..3])
    ```

5.
    ```haskell
    e :: IO Integer
    e = let ioi = readIO "1" :: IO Integer
            changed = fmap (read . ("123" ++) . show) ioi
        in fmap (*3) changed
    ```

## Exercises: Instances of Func

```haskell
module Spec where


import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Function as QF


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> QF.Fun a b -> QF.Fun b c -> Bool
functorCompose x (QF.Fun _ f) (QF.Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = QF.Fun Int Int
```

1.
    ```haskell
    newtype Identity a = Identity a deriving (Eq, Show)

    instance Functor Identity where
        fmap f (Identity a) = Identity $ f a

    instance Q.Arbitrary a => Q.Arbitrary (Identity a) where
        arbitrary = do
            a <- Q.arbitrary
            return $ Identity a

    type IntIdentityIdentity = Identity Int -> Bool
    type IntIdentityCompose = Identity Int -> IntToInt -> IntToInt -> Bool


    main :: IO ()
    main = do
        Q.quickCheck (functorIdentity :: IntIdentityIdentity)
        Q.quickCheck (functorCompose :: IntIdentityCompose)
    ```

2.
    ```haskell
    data Pair a = Pair a a deriving (Eq, Show)

    instance Functor Pair where
        fmap f (Pair a a') = Pair (f a) (f a')

    instance Q.Arbitrary a => Q.Arbitrary (Pair a) where
        arbitrary = do
            a <- Q.arbitrary
            a' <- Q.arbitrary
            return $ Pair a a'

    type IntPairIdentity = Pair Int -> Bool
    type IntPairCompose = Pair Int -> IntToInt -> IntToInt -> Bool


    main :: IO ()
    main = do
        Q.quickCheck (functorIdentity :: IntPairIdentity)
        Q.quickCheck (functorCompose :: IntPairCompose)
    ```

3.
    ```haskell
    data Two a b = Two a b deriving (Eq, Show)

    instance Functor (Two a) where
        fmap f (Two a b) = Two a (f b)

    instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (Two a b) where
        arbitrary = do
            a <- Q.arbitrary
            b <- Q.arbitrary
            return $ Two a b

    type IntTwoIdentity = Two Char Int -> Bool
    type IntTwoCompose = Two Char Int-> IntToInt -> IntToInt -> Bool


    main :: IO ()
    main = do
        Q.quickCheck (functorIdentity :: IntTwoIdentity)
        Q.quickCheck (functorCompose :: IntTwoCompose)
    ```

4.
    ```haskell
    data Three a b c = Three a b c deriving (Eq, Show)

    instance Functor (Three a b) where
        fmap f (Three a b c) = Three a b (f c)

    instance (Q.Arbitrary a, Q.Arbitrary b, Q.Arbitrary c) => Q.Arbitrary (Three a b c) where
        arbitrary = do
            a <- Q.arbitrary
            b <- Q.arbitrary
            c <- Q.arbitrary
            return $ Three a b c

    type IntThreeIdentity = Three Char Char Int -> Bool
    type IntThreeCompose = Three Char Char Int-> IntToInt -> IntToInt -> Bool


    main :: IO ()
    main = do
    ```

5.
    ```haskell
    data Three' a b = Three' a b b deriving (Eq, Show)

    instance Functor (Three' a) where
        fmap f (Three' a b b') = Three' a (f b) (f b')

    instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (Three' a b) where
        arbitrary = do
            a <- Q.arbitrary
            b <- Q.arbitrary
            b' <- Q.arbitrary
            return $ Three' a b b'

    type IntThree'Identity = Three' Char Int -> Bool
    type IntThree'Compose = Three' Char Int -> IntToInt -> IntToInt -> Bool


    main :: IO ()
    main = do
        Q.quickCheck (functorIdentity :: IntThree'Identity)
        Q.quickCheck (functorCompose :: IntThree'Compose)
    ```

6.
    ```haskell
    data Four a b c d = Four a b c d deriving (Eq, Show)

    instance Functor (Four a b c) where
        fmap f (Four a b c d) = Four a b c (f d)

    instance (Q.Arbitrary a, Q.Arbitrary b, Q.Arbitrary c, Q.Arbitrary d) =>
        Q.Arbitrary (Four a b c d) where
        arbitrary = do
            a <- Q.arbitrary
            b <- Q.arbitrary
            c <- Q.arbitrary
            d <- Q.arbitrary
            return $ Four a b c d

    type IntFourIdentity = Four Char Char Char Int -> Bool
    type IntFourCompose = Four Char Char Char Int -> IntToInt -> IntToInt -> Bool


    main :: IO ()
    main = do
        Q.quickCheck (functorIdentity :: IntFourIdentity)
        Q.quickCheck (functorCompose :: IntFourCompose)
    ```

7.
    ```haskell
    data Four' a b = Four' a a a b deriving (Eq, Show)

    instance Functor (Four' a) where
        fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

    instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (Four' a b) where
        arbitrary = do
            a <- Q.arbitrary
            a' <- Q.arbitrary
            a'' <- Q.arbitrary
            b <- Q.arbitrary
            return $ Four' a a' a'' b

    type IntFour'Identity = Four' Char Int -> Bool
    type IntFour'Compose = Four' Char Int -> IntToInt -> IntToInt -> Bool


    main :: IO ()
    main = do
        Q.quickCheck (functorIdentity :: IntFour'Identity)
        Q.quickCheck (functorCompose :: IntFour'Compose)
    ```

8. It cannot be done, `Trivial` is of kind `*` and Functor needs a kind `* -> *`


## Exercise: Possibly

```haskell
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap = undefined
```

## Short Exercise

1.
    ```haskell
    data Sum a b = First a | Second b deriving (Eq, Show)

    instance Functor (Sum a) where
        fmap f (Second b) = Second $ f b
        fmap _ (First a) = First a
    ```

2. Because that ...


## Chapter exercises

### Can a valid functor be written?

1. No

2. Yes

3. Yes

4. Yes

5. No

### Make the instace work
```haskell
{-# LANGUAGE FlexibleInstances #-}
```
1.
    ```haskell
    data Sum a b = First b | Second a
    instance Functor (Sum e) where
        fmap f (First a) = First (f a)
        fmap f (Second b) = Second b
    ```

2.
    ```haskell
    data Company a b c = DeepBlue a b | Something c
    instance Functor (Company e e') where
        fmap f (Something b) = Something (f b)
        fmap _ (DeepBlue a c) = DeepBlue a c
    ```

3.
    ```haskell
    data More a b = L b a b | R a b a deriving (Eq, Show)
    instance Functor (More x) where
        fmap f (L a b a') = L (f a) b (f a')
        fmap f (R b a b') = R b (f a) b'
    ```

1.
    ```haskell
    data Quant a b = Finance | Desk a | Bloor b
    instance Functor (Quant a) where
        fmap _ Finance = Finance
        fmap _ (Desk a) = Desk a
        fmap f (Bloor b) = Bloor $ f b
    ```

2.
    ```haskell
    data K a b = K a
    instance Functor (K a) where
        fmap _ (K a) = K a
    ```

3.
    ```haskell
    newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
    newtype K a b = K a

    -- should remind you of an
    -- instance you've written before
    instance Functor (Flip K a) where
        fmap f (Flip (K b)) = Flip $ K (f b)
    ```

4.
    ```haskell
    data EvilGoateeConst a b = GoatyConst b

    -- You thought you'd escaped the goats
    -- by now didn't you? Nope.
    instance Functor (EvilGoateeConst a) where
        fmap f (GoatyConst b) = GoatyConst $ f b
    ```

5.
    ```haskell
    data LiftItOut f a = LiftItOut (f a)

    instance Functor f => Functor (LiftItOut f) where
        fmap g (LiftItOut f) = LiftItOut $ fmap g f
    ```

6.
    ```haskell
    data Parappa f g a = DaWrappa (f a) (g a)
    instance (Functor f, Functor g) => Functor (Parappa f g) where
        fmap h (DaWrappa f g) = DaWrappa (fmap h f) (fmap h g)
    ```

7.
    ```haskell
    data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    instance Functor g => Functor (IgnoreOne f g a) where
        fmap h (IgnoringSomething f g) = IgnoringSomething f $ fmap h g
    ```

8.
    ```haskell
    data Notorious g o a t = Notorious (g o) (g a) (g t)
    instance Functor g => Functor (Notorious g o a) where
        fmap f (Notorious o a t) = Notorious o a (fmap f t)
    ```

9.
    ```haskell
    data List a = Nil | Cons a (List a)
    instance Functor List where
        fmap _ Nil = Nil
        fmap f (Cons a l) = Cons (f a) (fmap f l)
    ```

10.
    ```haskell
    data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    instance Functor GoatLord where
        fmap _ NoGoat = NoGoat
        fmap f (OneGoat g) = OneGoat $ f g
        fmap f (MoreGoats g g' g'') = MoreGoats (fmap f g) (fmap f g') (fmap f g'')
    ```

11.
    ```haskell
    data TalkToMe a = Halt | PrintString a | Read (String -> a)
    instance Functor TalkToMe where
        fmap _ Halt = Halt
        fmap f (PrintString a) = PrintString (f a)
        fmap f (Read g) = Read (f . g)
    ```