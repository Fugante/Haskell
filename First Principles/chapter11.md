# Chapter 11 exercises

## Exercises: Dog Types

```haskell
data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)
```

1. Type constructor

2. `* -> *`

3. `*`

4. `Num a => Doggies a`

5. `Integer a => Doggies Integer`

6. `Doggies String`

7. Both

8. `doge -> DoggeDeBordeaux doge`

9. `DogueDeBordeaux String`


## Exercises: Vehicles

```haskell
data Price = Price Integer deriving(Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving(Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving(Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline deriving(Eq, Show)


myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAi
```

1. `Vehicle`

2.
    ```haskell
    isCar :: Vehicle -> Bool
    isCar (Car _ _) = True
    isCar _ = False

    isPlane :: Vehicle -> Bool
    isPlane (Plane _) = True
    isPlane _ = False

    areCars :: [Vehicle] -> [Bool]
    areCars = map isCar
    ```

3.
    ```haskell
    getManu :: Vehicle -> Manufacturer
    getManu (Car manufacturer _) = manufacturer
    ```

4. It will raise an exception.

5.
    ```haskell
    data Size = Size Integer deriving (Eq, Show)

    data Vehicle' = Car' Manufacturer Price | Plane' Airline Size deriving(Eq, Show)

    isPlane' :: Vehicle' -> Bool
    isPlane' (Plane' _ _) = True
    isPlane' _ = False
    ```


## Exercises: Cardinality

1. 1

2. 3

3. 65,536

4. The cardinality of `Int` is 18,446,744,073,709,551,616. `Integer` is unbounded.

5. 2^8 = 256


## Exercises: For Example

```haskell
data Example = MakeExample deriving Show
```

1. The type of `MakeExample` is `Example`. An error is raised when trying to evaluate the
type of `MakeExample`.

2. Yes, you can see the instances of `Example` by using `:info Example`. In this case
there is an instance for `Show`.

3.
    ```haskell
    data Example' = MakeExample' Int deriving Show
    ```
    The type of `MakeExample'` is `Int -> Example'`. The type of `MakeExample' 1` is `Example'`.

## Exercises: Logic Goats

```haskell
{-# LANGUAGE FlexibleInstances #-}


class TooMany a where
    tooMany:: a-> Bool

instance TooMany Int where
    tooMany n = n > 42
```

1.
    ```haskell
    instance TooMany (Int, String) where
        tooMany (n, _) = tooMany n
    ```

2.
    ```haskell
    instance TooMany (Int, Int) where
        tooMany (n, m) = tooMany (n + m)
    ```

3.
    ```haskell
    instance (Num a, TooMany a) => TooMany (a, a) where
        tooMany (n, m) = tooMany (n + m)
    ```


## Exercises: Pity the Bool

1.
    ```haskell
    data BigSmall =
        Big Bool | Small Bool
        deriving (Eq, Show)
    ```
    The cardinality is 4 (2 + 2)

2.
    ```haskell
    import Data.Int

    data NumberOrBool =
        Numba Int8 | BoolyBool Bool
        deriving(Eq, Show)

    -- parentheses due to syntactic
    -- collision between (-) minus
    -- and the negate function
    myNumba = Numba(-128)
    ```
    The cardinality of `NumberOrBool` is 258 (256 + 2).

    It overflows and a takes a new value.

    Same as the previous case.


## Exercises: How Does Your Garden Grow?

1.
    ```haskell
    data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show

    type Gardener = String

    data Garden = Garden Gardener FlowerType derivingShow
    ```
    ```haskell
    data Garden' = GardenG Gardener Gardenia
                 | GardenD Gardener Daisy
                 | GardenR Gardener Rose
                 | GardenL Gardener Lilac
    ```


## Exercises: Programmers

```haskell
data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgLang = Haskell
              | Agda
              | Idris
              | PureScript
              deriving (Eq, Show)

data Programmer = Programmer
                  {
                    os   :: OperatingSystem,
                    lang :: ProgLang
                  }
                  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [
        GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows
    ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]
```

```haskell
allProgrammers :: [Programmer]
allProgrammers = [
        Programmer os lang | os <- allOperatingSystems, lang <- allLanguages
    ]
```

## Exercises: The Quad

```haskell
data Quad = One | Two | Three | Four deriving (Eq, Show)
```

1.
    ```haskell
    eQuad :: Either Quad Quad
    eQuad = ???
    ```
    2 + 2 = 4

2.
    ```haskell
    prodQuad :: (Quad, Quad)
    ```
    2 * 2 = 4

3.
    ```haskell
    funcQuad :: Quad -> Quad
    ```
    4 * 4 = 16

4.
    ```haskell
    prodTBool :: (Bool, Bool, Bool)
    ```
    2 * 2 * 2 = 8

5.
    ```haskell
    gTwo :: Bool -> Bool -> Bool
    ```
    (2²)² = 2⁽²*²⁾ = 2⁴ = 16

6.
    ```haskell
    fTwo :: Bool -> Quad -> Quad
    ```
    (4⁴)² = 4⁽⁴*²⁾ = 4⁽⁸⁾ = 65536


## Write map for BinaryTree

```haskell
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)
```

```haskell
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)
```


## Convert binary trees to lists

```haskell
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = preorder left ++ a : preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = a : preorder right ++ preorder left
```

## Write foldr for BinaryTree

```haskell
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b = foldr f b . inorder
```

## Chapter Exercises

### Multiple Choice

1.
    ```haskell
    data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday
    ```
    a. Weekday is a type with five data constructors

2.
    ```haskell
    f Friday = "Miller Time"
    ```
    c. `f :: Weekday -> String`

3. Types defined with the data keyword

    b. must begin with a capital letter

4. The function `g xs = xs !! (length xs - 1)`

    c. delivers the final element of xs

### Ciphers

```haskell
import Data.Char (ord, toLower, chr)


aValue :: Int
aValue = ord 'a'

toLowercase :: String -> String
toLowercase = map toLower

letterToInt :: Char -> Int
letterToInt = flip (-) aValue . ord

intToLetter :: Int -> Char
intToLetter = chr . (+) aValue

numLetters = letterToInt 'z'

shiftLetter :: Char -> Int -> Char
shiftLetter letter shift = intToLetter $ (letterToInt letter + shift) `mod` numLetters

_encryptText :: String -> String -> String
_encryptText "" text = text
_encryptText key text = go key text
    where
        go :: String -> String -> String
        go _ "" = ""
        go "" text = go key text
        go (k:ks) (t:ts) = shiftLetter t (letterToInt k) : go ks ts

encrypt :: String -> String -> String
encrypt key text = _encryptText (toLowercase key) (toLowercase text)
```


### As-patterns

1.
    ```haskell
    isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
    isSubseqOf [] _ = True
    isSubseqOf _ [] = False
    isSubseqOf sub@(s:ss) (t:ts)
        | s == t = isSubseqOf ss ts
        | otherwise = isSubseqOf sub ts
    ```

2.
    ```haskell
    import Data.Char (toUpper)


    capitalizeWords :: String -> [(String, String)]
    capitalizeWords = map (\word@(w:ws) -> (word, toUpper w: ws)) . words
    ```


### Language exercises

1.
    ```haskell
    import Data.Char (toUpper)


    capitalizeWord :: String -> String
    capitalizeWord (w:ws) = toUpper w : ws
    ```

2.
    ```haskell
    capitalizeParagraph :: String -> String
    capitalizeParagraph = foldr f "" . words
        where
            f :: String -> String -> String
            f word "" = capitalizeWord word
            f word sentence
                | last sentence == '.' = sentence ++ ' ' : capitalizeWord word
                | otherwise = sentence ++ ' ' : word
    ```

### Phone exercise

TODO

### Hutton’s Razor

```haskell
data Expr = Lit Integer | Add Expr Expr
```

1.
    ```haskell
    eval :: Expr -> Integer
    eval (Lit integer) = integer
    eval (Add expr1 expr2) = eval expr1 + eval expr2
    ```
2.
    ```haskell
    printExpr :: Expr -> String
    printExpr (Lit integer) = show integer
    printExpr (Add expr1 expr2) = printExpr expr1 ++ " + " ++ printExpr expr2
    ```