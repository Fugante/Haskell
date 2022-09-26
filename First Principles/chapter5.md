# Chapter 5 excercises

## Type Matching

```
not :: Bool -> Bool

length :: [a] -> Int

concat :: [[a]] -> [a]

head :: [a] -> a

(<) :: Ord a => a -> a -> Bool
```

## Type Arguments

1.
        f :: a -> a -> a -> a
        x :: Char
    * a. `Char -> Char -> Char`

2.
        g :: a -> b -> c -> b
        g 0 'c' "woot"
    * d. `Char`

3.
        h :: (Num a, Num b) => a -> b -> b
        h 1.0 2
    * d. `Num b => b`

4.
        h :: (Num a, Num b) => a -> b -> b
        h 1 (5.5 :: Double)
    * c. `Double`

5.
        jackal :: (Ord a, Eq b) => a -> b -> a
        jackal "keyboard" "has the word jackal in it"
    * a. `[Char]`

6.
        jackal :: (Ord a, Eq b) => a -> b -> a
        jackal "keyboard"
    * d. `Eq b => b -> [Char]`

7.
        kessel :: (Ord a, Num b) => a -> b -> a
        kessel 1 2
    * d. `(Ord a, Num a) => a`

8.
        kessel :: (Ord a, Num b) => a -> b -> a
        kessel 1 (2 :: Integer)
    * a. `(Ord a, Num a) => a`

9.
        kessel :: (Ord a, Num b) => a -> b -> a
        kessel (1 :: Integer) 2
    * c. `Integer`

## Parametricity

2.
        f :: a -> a -> a
        -- a.
        f _ a = a
        -- b.
        f a _ = a

3.
        f :: a -> b -> b
        f _ b = b


## Apply yourself

1.
        (++) :: [a] -> [a] -> [a]
        myConcat x = x ++ "yo"
        -- myConcat :: [Char] -> [Char]

2.      (*) :: Num a => a -> a -> a
        myMult x = (x / 3) * 5
        -- myMult :: Fractional a => a -> a

3.      take :: Int -> [a] -> [a]
        myTake x = take x "hey you"
        -- myTake :: Int -> [Char]

4.      (>) :: Ord a => a -> a -> Bool
        myCom x = x > (length [1..10])
        -- myCom :: Int -> Bool

5.      (<) :: Ord a => a -> a -> Bool
        myAlph x = x < 'z'
        -- myAlph :: Char -> Bool

## Chapter excercises
### Multiple choice

1. A value of type `[a]` is
    * c. a list whose elements are all of the some type `a`

2. A function of type `[[a]] -> [a]` could
    * a. take a list of strings as an argument

3. A function of type `[a] -> Int -> a`
    * b. returns one element of type a from a list

4. A function of type `(a, b) -> a`
    * c. takes a tuple argument and returns the first value

### Determine the type

1.
    * a. `Num a => a`
    * b. `Num a => (a, [Char])`
    * c. `(Integer, [Char])`
    * d. `Bool`
    * e. `Int`
    * f. `Bool`

2.      x = 5
        y = x + 5
        w = y * 10
        -- w :: Num a => a

3.      x = 5
        y = x + 5
        z y = y * 10
        -- z :: Num a => a -> a

4.      x = 5
        y = x + 5
        f = 4 / y
        -- f :: Fractional a => a

5.      x = "Julie"
        y = " <3 "
        z = "Haskell"
        f = x ++ y ++ z
        -- f :: [Char]


### Does it complile?

1. wahoo. You cannot apply a value to another value

2. None

3. c and d. You cannot apply a value to another value

4. b will fail. c is not defined


### Type variable or specific type constructor?

1.  constrained polymorphic: 0

    fully polymorphic: 1

    concrete: 2, 3

2.  concrete: 0, 1

3.  fully polymorphic: 0

    constrained polymorphic: 1

    concrete: 2

4.  fully polymorphic: 0, 1

    concrete: 2


### Write a type signature

1.      functionH :: [a] -> a

2.      functionC :: Ord a => a -> a -> Bool

3.      functionS :: (a, b) -> b


### Given a type, write the function

1.      i :: a -> a
        i a = a

2.      c :: a -> b -> a
        c a b = a

3.      c'' :: b -> a -> b
        c'' b a = b
        -- It is the same function

4.      c' :: a -> b -> b
        c' a b = b

5.      r :: [a] -> [a]
        r (x:_) = [x]

6.      co :: (b -> c) -> (a -> b) -> a -> c
        co bToC aToB a = bToC (aToB a)

7.      a :: (a -> c) -> a -> a
        a _ x = x

8.      a' :: (a -> b) -> a -> b
        a' aToB a = aToB a


### Fix it

1.      module sing where

        fstString :: [Char] -> [Char]
        fstString x = x ++ " in the rain"

        sndString :: [Char] -> [Char]
        sndString x = x ++ " over the rainbow"

        sing = if (x > y) then fstString x else sndString y
            where x = "Singin"
                  y = "Somewhere"

2.      sing = if (x < y) then fstString x else sndString y
            where x = "Singin"
                  y = "Somewhere"

3.      module Arith3Broken where

        main :: IO ()
        main = do
            print (1 + 2)
            print 10
            print (negate (-1))
            print ((+) 0 blah)
            where blah = negate 1

### Type-Kwon-Do

1.      f :: Int -> String
        f = undefined

        g :: String -> Char
        g = undefined

        h :: Int -> Char
        h n = g (f n)

2.      data A
        data B
        data C

        q :: A -> B
        q = undefined

        w :: B -> C
        w = undefined

        e :: A -> C
        e a = w (q a)

3.      data X
        data Y
        data Z

        xz :: X -> Z
        xz = undefined

        yz :: Y -> Z
        yz = undefined

        xform :: (X, Y) -> (Z, Z)
        xform (x, y) = (xz x, yz y)

4.      munge :: (x -> y) -> (y -> (w, z)) -> x -> w
        munge x2y y2wz x = fst (y2wz (x2y x))