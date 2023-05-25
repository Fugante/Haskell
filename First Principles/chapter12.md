# Chapter 11 exercises

## Chapter Exercises

### Determine the kinds

1. `id :: a -> a`
    The kind of `a` is `*`

2. `r :: a -> f a`
    The kind `a` is `*`, of `f` is `* -> *`


## String processing

1.
    ```haskell
    notThe :: String -> Maybe String
    notThe "the" = Nothing
    notThe str = Just str

    replaceThe :: String -> String
    replaceThe = unwords . map replace . words
        where
            replace :: String -> String
            replace word = if notThe word == Nothing then "a" else word
    ```

2.
    ```haskell
    vowels :: [Char]
    vowels = "aiueo"

    countTheBeforeVowel :: String -> Integer
    countTheBeforeVowel = go 0 . words
        where
            go :: Integer -> [String] -> Integer
            go i [] = i
            go i (_:[]) = go i []
            go i ("the":ws:wss) = if head ws `elem` vowels then go (i + 1) wss else go i wss
            go i (_:ws) = go i ws
    ```

3.
    ```haskell
    countVowels :: String -> Integer
    countVowels = foldr (\l i -> if l `elem` vowels then i + 1 else i) 0
    ```


## Validate the word

```haskell
newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord word = if fst countTuple > snd countTuple then Nothing else Just (Word' word)
    where
        countTuple :: (Integer, Integer)
        countTuple = foldr count (0,0) word

        count :: Char -> (Integer, Integer) -> (Integer, Integer)
        count letter (v,c)
            | letter `elem` vowels = (v + 1, c)
            | otherwise = (v, c + 1)
```


## It's only natural

```haskell
natToInteger :: Nat -> Integer
natToInteger nat = go nat 0
    where
        go :: Nat -> Integer -> Integer
        go Zero i = i
        go (Succ n) i = go n (i + 1)

integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i < 0 = Nothing
    | otherwise = Just (getNat i)
        where
            getNat :: Integer -> Nat
            getNat 0 = Zero
            getNat i = Succ (getNat $ i - 1)
```


## Simple library for Maybe

1.
    ```haskell
    isJust :: Maybe a -> Bool
    isJust Nothing = False
    isJust _ = True

    isNothing :: Maybe a -> Bool
    isNothing Nothing = True
    isNothing _ = False
    ```
2.
    ```haskell
    maybee :: b -> (a -> b) -> Maybe a -> b
    maybee b _ Nothing = b
    maybee b f (Just a) = f a
    ```

3.
    ```haskell
    fromMaybe :: a -> Maybe a -> a
    fromMaybe a = maybee a id
    ```

4.
    ```haskell
    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x:xs) = Just x

    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just a) = [a]
    ```

5.
    ```haskell
    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr go []
        where
            go :: Maybe a -> [a] -> [a]
            go Nothing list = list
            go (Just a) list = a:list
    ```

6.
    ```haskell
    flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe maybeList = go maybeList (Just [])
    where
        go :: [Maybe a] -> Maybe [a] -> Maybe [a]
        go [] list = list
        go (Nothing : xs) _ = Nothing
        go (Just x : xs) (Just ys) = go xs $ Just (ys ++ [x])
    ```


## Small library for Either

1.
    ```haskell
    lefts' :: [Either a b] -> [a]
    lefts' = foldr chooseLeft []
        where
            chooseLeft (Left a) acc = a : acc
            chooseLeft (Right b) acc = acc
    ```

2.
    ```haskell
    rights' :: [Either a b] -> [b]
    rights' = foldr chooseRight []
        where
            chooseRight (Left a) acc = acc
            chooseRight (Right b) acc = b : acc
    ```

3.
    ```haskell
    partitionEithers' :: [Either a b] -> ([a], [b])
    partitionEithers' = foldr partition ([], [])
        where
            partition (Left a) (ls, rs) = (a : ls, rs)
            partition (Right b) (ls, rs) = (ls, b : rs)
    ```

4.
    ```haskell
    eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe' f (Right b) = Just $ f b
    eitherMaybe' _ _ = Nothing
    ```

5.
    ```haskell
    either' :: (a -> c) -> (b -> c) -> Either a b -> c
    either' f _ (Left a) = f a
    either' _ g (Right b) = g b
    ```

6.
    ```haskell
    eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe'' f = either' (const Nothing) (Just . f)
    ```


## Write your own iterate and unfoldr

1.
    ```haskell
    myIterate :: (a -> a) -> a -> [a]
    myIterate f a = a : myIterate f (f a)
    ```

2.
    ```haskell
    myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    myUnfoldr f b =
        case f b of
            Nothing -> []
            Just (a', b') -> a' : myUnfoldr f b'
    ```

3.
    ```haskell
    betterIterate :: (a -> a) -> a -> [a]
    betterIterate f = myUnfoldr (\b -> Just (b, f b))
    ```


## Finally something other than a list!

```haskell
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)
```

1.
    ```haskell
    unfold :: (b -> Maybe (b, a, b)) -> b -> BinaryTree a
    unfold f b =
        case f b of
            Nothing -> Leaf
            Just (b', a, b'') -> Node (unfold f b') a (unfold f b'')
    ```

2.
    ```haskell
    treeBuild :: Integer -> BinaryTree Integer
    treeBuild n = unfold builder 0
        where
            builder :: Integer -> Maybe (Integer, Integer, Integer)
            builder m
                | m == n = Nothing
                | otherwise = Just (m + 1, m, m + 1)
    ```