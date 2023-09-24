# Chapter 7 excercises

## Grab bag

1. all are equivalent

2. d) `Num a => a -> a -> a`

3.
    a.
    ```haskell
    f n = n + 1
    \n -> n + 1`
    ```

    b.
    ```haskell
    addFive x y = (if x > y then y else x) + 5
    \x -> \y -> (if x > y then y else x) + 5
    ```

    c.
    ```haskell
    mflip f = \x -> \y -> f y x
    mflip f x y = f y x
    ```


## Variety pack

1.
    ```haskell
    k (x, y) = x
    k1 = k ((4 - 1), 10)
    k2 = k ("three", (1 + 2))
    k3 = k (3, True)
    ```

    a. `k :: (a, b) -> a`
    
    b. `k2 :: [Char]`. It's different to k1 and k3.
    
    c. k1 and k3.

2.
    ```haskell
    f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
    f (a, b, c) (d, e, f) = ((a, d), (c, f))
    ```

## Case practice

1.      
    ```haskell
    functionC x y = if (x > y) then x else y
    functionC' x y = case (x > y) of
        True -> x
        False -> y
    ```

2.      
    ```haskell
    ifEvenAdd2 n = if even n then (n + 2) else n
    ifEvenAdd2' n = case even n of
        True -> n + 2
        False -> n
    ```

3.
    ```haskell
    nums x = case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0
    ```


## Artful Dodgy

```haskell
dodgy :: Num a => a -> a -> a
dodgy x y  = x + y * 10

oneIsOne :: Num a => a -> a 
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2
```

1.
    ```haskell
    dodgy 1 0   -- 1
    ```

2.
    ```haskell
    dodgy 1 1   -- 11
    ```

3.
    ```haskell
    dodgy 2 2   -- 22
    ```

4.
    ```haskell
    dodgy 1 2   -- 21
    ```

5.
    ```haskell
    dodgy 2 1   -- 12
    ```

6.
    ```haskell
    oneIsOne 1  -- 11
    ```

7.
    ```haskell
    oneIsOne 2  -- 21
    ```

8.
    ```haskell
    oneIsTwo 1  -- 21
    ```

9.
    ```haskell
    oneIsTwo 2  -- 22
    ```

10.
    ```haskell
    oneIsOne 3  -- 31
    ```

11.
    ```haskell
    oneIsTwo 3  -- 23
    ```

## Guard duty

```haskell
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100
```

1. It will always evaluate to an `F`

2. Depending on where you place `y >= 0.7 = 'C'`, it will evaluate to a lower or higher score than the correct one.

3. b) True when a `xs` is a palindrome

4. A list of any type

5. `avgGrade :: Eq a => [a] -> Bool`

6. c) an indication of whether its argument is a positive or negative number or zero

7. Any numerical value

8. `numbers :: (Num a, Ord a, Num b) => a -> b`


## Chapter excercises

### Multiple choice

1. A polymorphic function

    d) may resolve to values of different types, depending on the inputs 

2. Two functions named `f` and `g` have types `Char -> String` and `String -> [String]` respectively. The composed function `g . f` has the type

    d) `Char -> [String]`

3. A function `f` has the type `Ord a => a -> a -> Bool` and we apply it to one numeric value. What is the type now?

    d)  `(Ord a, Num a) => a -> Bool`

4. A function with the type `(a -> b) -> c`

    b) is a higher order function

5. Given the following `f`, what is the type of `f True`?
    ```haskell
    f :: a -> a
    f x = x
    ```
    a) `f True :: Bool` 


### Let's write code

1.
    ```haskell
    tensDigit :: Integral a => a -> a
    tensDigit x = d 
        where xLast = x `div` 10
                d = xLast `mod` 10
    ```
    
    a.
    ```haskell
    tensDigit x = (fst (divMod x 10)) `mod` 10
    ```

    b. Yes

    c. 
    ```haskell
    hunsD = (`mod` 100) . (`div` 100)
    ```

2.
    ```haskell
    foldBool :: a -> a -> Bool -> a
    foldBool x y b = case b of
        False -> x
        True -> y

    foldBool2 :: a -> a -> Bool -> a
    foldBool2 x y b
        | b = y
        | otherwise = x

    foldBool3 :: a -> a -> Bool -> a
    foldBool3 x _  False = x
    foldBool3 _ y True = True
    ```

3.
    ```haskell
    g :: (a -> b) -> (a, c) -> (b, c)
    g f (a, c) = (f a, c)
    ```

4.
    ```haskell
    roundTrip :: (Show a, Read a) => a -> a
    roundTrip a = read (show a)

    main = do
        print(roundTrip 4)
        print(id 4)
    ```

5.
    ```haskell
    roundTrip :: (Show a, Read a) => a -> a
    roundTrip = read . show
    ```

6.
    ```haskell
    roundTrip :: (Show a, Read b) => a -> b
    roundTrip a = read (show a) :: Float
    ```