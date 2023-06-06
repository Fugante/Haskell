# Chapter 15 exercises

## Exercise: Optional Monoid

```haskell
data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) :: Semigroup a => Optional a -> Optional a -> Optional a
    (<>) (Only a) (Only b) = Only (a <> b)
    (<>) a Nada = a
    (<>) Nada b = b

instance Monoid a => Monoid (Optional a) where
    mempty :: Monoid a => Optional a
    mempty = Nada
```


## Madness

```haskell
import Data.Monoid


type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
    e <> "! he said "
    <> adv <> " as he jumped into his car "
    <> noun <> " and drove off with his "
    <> adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
    e `mappend` "! he said "
    `mappend` adv `mappend` " as he jumped into his car "
    `mappend` noun `mappend` " and drove off with his "
    `mappend` adj `mappend` " wife."
```


## Exercise: Maybe Another Monoid

```haskell
data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = undefined
    mappend = undefined

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
```


## Chapter Exercises

### Semigroup exercises

1.
    ```haskell
    import qualified Data.Monoid as M
    import qualified Data.Semigroup as S
    import Test.QuickCheck


    data Trivial = Trivial deriving (Eq, Show)

    instance Semigroup Trivial where
        _ <> _  = Trivial

    instance Arbitrary Trivial where
        arbitrary = return Trivial

    semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
    semigroupAssoc a b c = (a <> (b <> c)) == (( a <> b) <> c)

    type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

    main :: IO ()
    main = quickCheck (semigroupAssoc :: TrivAssoc)
    ```

2.
    ```haskell
    newtype Identity a = Identity a

    instance Eq a => Eq (Identity a) where
        (Identity a) == (Identity b) = a == b

    instance Show a => Show (Identity a) where
        show (Identity a) = "Identity " ++ show a

    instance Semigroup a => Semigroup (Identity a) where
        (Identity a) <> (Identity b) = Identity $ a <> b


    instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = do
            a <- arbitrary
            return $ Identity a

    identityAssoc :: Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool
    identityAssoc = semigroupAssoc


    main :: IO ()
    main = quickCheck identityAssoc
    ```

3.
    ```haskell
    data Two a b = Two a b deriving (Show, Eq)

    instance (S.Semigroup a, S.Semigroup b) => Semigroup (Two a b) where
        Two a b <> Two a' b' = Two (a <> a') (b <> b')

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return $ Two a b

    twoAssoc :: Two [Int] [Int] -> Two [Int] [Int] -> Two [Int] [Int] -> Bool
    twoAssoc = semigroupAssoc
    ```

4.
    ```haskell
    data Three a b c = Three a b c deriving (Show, Eq)

    instance (S.Semigroup a, S.Semigroup b, S.Semigroup c) => Semigroup (Three a b c) where
        Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

    instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return $ Three a b c

    threeAssoc ::
        Three [Int] [Int] [Int] ->
        Three [Int] [Int] [Int] ->
        Three [Int] [Int] [Int] ->
        Bool
    threeAssoc = semigroupAssoc
    ```
5.
    ```haskell
    data Four a b c d = Four a b c d deriving (Show, Eq)

    instance
        (S.Semigroup a, S.Semigroup b, S.Semigroup c, Semigroup d) =>
        Semigroup (Four a b c d) where
        Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')

    instance
        (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
        Arbitrary (Four a b c d) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return $ Four a b c d

    fourAssoc ::
        Four [Int] [Int] [Int] [Int] ->
        Four [Int] [Int] [Int] [Int] ->
        Four [Int] [Int] [Int] [Int] ->
        Bool
    fourAssoc = semigroupAssoc
    ```

6.
    ```haskell
    newtype BoolConj = BoolConj Bool deriving (Eq, Show)

    instance Semigroup BoolConj where
        BoolConj True <> BoolConj True = BoolConj True
        _ <> _ = BoolConj False

    instance Arbitrary BoolConj where
        arbitrary = do
            b <- arbitrary
            return $ BoolConj b

    type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
    ```

7.
    ```haskell
    newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

    instance Semigroup BoolDisj where
        BoolDisj False <> BoolDisj False = BoolDisj False
        _ <> _ = BoolDisj True

    instance Arbitrary BoolDisj where
        arbitrary = do
            b <- arbitrary
            return $ BoolDisj b

    type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
    ```

8.
    ```haskell
    data Or a b = Fst a | Snd b deriving (Eq, Show)

    instance Semigroup (Or a b) where
        Snd b <> _ = Snd b
        _ <> Snd b = Snd b
        _ <> b = b

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            elements [return $ Fst a, return $ Snd b]
    ```

9.
    ```haskell
    newtype Combine a b = Combine { unCombine :: a -> b }

    instance (Num a, Num b) => Semigroup (Combine a b) where
        Combine f <> Combine g = Combine (\x -> f x <> g x)

    instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
        arbitrary = do
            f <- arbitrary
            return $ Combine f
    ```

10.
    ```haskell
    newtype Comp a = Comp { unComp :: a -> a }

    instance Semigroup a => Semigroup (Comp a) where
        Comp a <> Comp b = Comp $ a <> b

    instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
        arbitrary = do
            f <- arbitrary
            return $ Comp f
    ```

11.
    ```haskell
    data Validation a b = Failure' a | Success' b deriving (Eq, Show)

    instance Semigroup a => Semigroup (Validation a b) where
        Success' b <> _ = Success' b
        _ <> Success' b' = Success' b'
        Failure' a <> Failure' a' = Failure' $ a <> a'

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            oneof [return $ Failure' a, return $ Success' b]
    ```


### Monoid exercises

1.
    ```haskell
    data Trivial = Trivial deriving (Eq, Show)

    instance Semigroup Trivial where
        _ <> _ = Trivial

    instance Monoid Trivial where
        mempty = Trivial
        mappend = (<>)

    instance Arbitrary Trivial where
        arbitrary = return Trivial

    semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
    semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

    monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
    monoidLeftIdentity a = a <> mempty == a

    monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
    monoidRightIdentity a = mempty <> a == a

    type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool


    main :: IO ()
    main = do
        let sa = semigroupAssoc
            mli = monoidLeftIdentity
            mlr = monoidRightIdentity
        quickCheck (sa :: TrivAssoc)
        quickCheck (mli :: Trivial -> Bool)
        quickCheck (mlr :: Trivial -> Bool)
    ```

2.
    ```haskell
    newtype Identity a = Identity a deriving (Show, Eq)

    instance Semigroup a => Semigroup (Identity a) where
        Identity a <> Identity a' = Identity (a <> a')

    instance Monoid a => Monoid (Identity a) where
        mempty = Identity mempty
        mappend = (<>)

    instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = do
            a <- arbitrary
            return $ Identity a


    main :: IO ()
    main = do
        let sa = semigroupAssoc
            mli = monoidLeftIdentity
            mlr = monoidRightIdentity
        quickCheck (sa :: Identity Ordering-> Identity Ordering -> Identity Ordering -> Bool)
        quickCheck (mli :: Identity Ordering -> Bool)
        quickCheck (mlr :: Identity Ordering -> Bool)
    ```

3.
    ```haskell
    data Two a b = Two a b deriving (Show, Eq)

    instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
        Two a b <> Two a' b' = Two (a <> a') (b <> b')

    instance (Monoid a, Monoid b) => Monoid (Two a b) where
        mempty = Two mempty mempty
        mappend = (<>)

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return $ Two a b

    type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool
    type TwoLeftIdentity a b = Two a b -> Bool
    type TwoRightIdentity a b = Two a b -> Bool


    main :: IO ()
    main = do
        let sa = semigroupAssoc
            mli = monoidLeftIdentity
            mlr = monoidRightIdentity
        quickCheck (sa :: TwoAssoc Ordering Ordering)
        quickCheck (mli :: TwoLeftIdentity Ordering Ordering)
        quickCheck (mlr :: TwoRightIdentity Ordering Ordering)
    ```

5.
    ```haskell
    newtype BoolConj = BoolConj Bool deriving (Eq, Show)

    instance Semigroup BoolConj where
        BoolConj True <> BoolConj True = BoolConj True
        _ <> _ = BoolConj False

    instance Monoid BoolConj where
        mempty = BoolConj True
        mappend = (<>)

    instance Arbitrary BoolConj where
        arbitrary = do
            b <- arbitrary
            return $ BoolConj b

    type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
    type BoolConjLeftIdentity = BoolConj -> Bool
    type BoolConjRightIdentity = BoolConj -> Bool


    main :: IO ()
    main = do
        let sa = semigroupAssoc
            mli = monoidLeftIdentity
            mlr = monoidRightIdentity
        quickCheck (sa :: BoolConjAssoc)
        quickCheck (mli :: BoolConjLeftIdentity)
        quickCheck (mlr :: BoolConjRightIdentity)
    ```

6.
    ```haskell
    newtype Combine a b = Combine { unCombine :: a -> b }

    instance Semigroup b => Semigroup (Combine a b) where
        Combine f <> Combine g = Combine $ f <> g

    instance Monoid b => Monoid (Combine a b) where
        mempty = Combine mempty

    instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
        arbitrary = do
            f <- arbitrary
            return $ Combine f 
    ```

7.
    ```haskell
    newtype Comp a = Comp (a -> a)

    instance Semigroup a => Semigroup (Comp a) where
        Comp a <> Comp a' = Comp $ a <> a'

    instance Monoid a => Monoid (Comp a) where
        mempty = Comp id
        mappend = (<>)

    instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
        arbitrary = do
            f <- arbitrary
            return $ Comp f

    type CompAssoc a = Comp a -> Comp a -> Comp a -> Bool
    type CompLeftIdentity a = Comp a -> Bool
    type CompRightIdentity a = Comp a -> Bool
    ```

9.
    ```haskell
    newtype Mem s a = Mem { runMem :: s -> (a, s) }

    instance Semigroup a => Semigroup (Mem s a) where
        Mem f <> Mem g = Mem h
            where
                h :: Semigroup a => s -> (a, s)
                h s = (a' <> a'', s'')
                    where
                        (a', s') = f s
                        (a'', s'') = g s'

    instance Monoid a => Monoid (Mem s a) where
        mempty = Mem $ \s -> (mempty, s)
        mappend = (<>)


    f' = Mem $ \s -> ("hi", s + 1)

    main :: IO ()
    main = do
        let rmzero = runMem mempty 0
            rmleft = runMem (f' <> mempty) 0
            rmright = runMem (mempty <> f') 0
        print rmleft                            -- ("hi",1)
        print rmright                           -- ("hi",1)
        print (rmzero :: (String, Int))         -- ("",0)
        print $ rmleft == runMem f' 0           -- True
        print $ rmright == runMem f' 0          -- True
    ```