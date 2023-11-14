# Chapter 22 exercises

## Short Exercise: Warming Up

```haskell
cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . reverse

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    a <- cap
    b <- rev
    return (a, b)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= (\a -> rev >>= (\b -> return (a, b)))
```

## Exercise: Ask

```haskell
newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id
```

## Exercise: Reading Comprehension

1.
    ```haskell
    myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
    myLiftA2 f fa fb = f <$> fa <*> fb
    ```

2.
    ```haskell
    asks :: (r -> a) -> Reader r a
    asks f = Reader f
    ```

3.
    ```haskell
    instance Applicative (Reader r) where
        pure :: a -> Reader r a
        pure = Reader . const

        (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
        Reader rf <*> Reader ra = Reader $ \r -> rf r $ ra r
    ```

## Exercise Reader Monad

```haskell
newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person =
    Person
    {
          humanName :: HumanName
        , dogName :: DogName
        , address :: Address
    }
    deriving (Eq, Show)

data Dog =
    Dog
    {
          dogsName :: DogName
        , dogsAddress :: Address
    }
    deriving (Eq, Show)

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy
```

1.
    ```haskell
    instance Monad (Reader r) where
        return :: a -> Reader r a
        return = pure

        (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
        Reader ra >>= f = Reader $ \r -> runReader (f (ra r)) r
    ```

2.
    ```haskell
    getDogRM' :: Reader Person Dog
    getDogRM' = do
        n <- Reader dogName
        a <- Reader address
        return $ Dog n a

    getDogRM'' :: Reader Person Dog
    getDogRM'' = Reader dogName >>= (\n -> Reader address >>= \a -> return $ Dog n a)
    ```

## Chapter Exercises

### A warm-up stretch

```haskell
x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys
-- x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs
-- x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = liftA2 (,) z' z'
-- x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)
-- bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> ys)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequenceA [(>3), (<8), even] 7
```

1.
    ```haskell
    main :: IO ()
    main = do
        print $ foldr (&&) True (sequA 5)
    ```

2.
    ```haskell
    main :: IO ()
    main = do
        print $ sequA (fromMaybe 0 s')
    ```

3.
    ```haskell
    main :: IO ()
    main = do
        print $ bolt (fromMaybe 0 ys)
    ```