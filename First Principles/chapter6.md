# Chapter 6 excercises

## Eq instances

1.
        data TisAnInteger = TisAn Integer
        instance Eq TisAnInteger where
            TisAn n == TisAn m = n == m

2.
        data TwoIntegers = Two Integer Integer
        instance Eq TwoIntegers where
            Two n m == Two n' m' = n == n' && m == m'

3.
        data StringOrInt = TisAnInt Int | TisAString String
        instance Eq StringOrInt where
            TisAnInt n == TisAnInt n' = n == n'
            TisAString s == TisAString s' = s == s'

4.
        data Pair a = Pair a a
        instance Eq a => Eq (Pair a) where
            Pair a b == Pair a' b' = a == a' && b == b'

5.
        data Tuple a b = Tuple a b
        instance (Eq a, Eq b) => Eq (Tuple a b) where
            Tuple a b == Tuple a' b' = a == a' && b == b'

6.
        data Which a = ThisOne a | ThatOne a
        instance Eq a => Eq (Which a) where
            ThisOne a == ThisOne a' = a == a'
            ThatOne a == ThatOne a' = a == a'
            _ == _ = False

7.
        data EitherOr a b = Hello a | Goodbye b
        instance (Eq a, Eq b) => Eq (EitherOr a b) where
            Hello a == Hello a' = a == a'
            Goodbye a == Goodbye a' = a == a'
            _ == _ = False

## Will they work?

1.      max (length [1,2,3,4]) (length [8,9, 10, 11, 12]) 
        -- compiles

2.      compare (3 * 4) (3 * 5)
        -- compiles

3.      compare "Julie" True
        -- doesn't compile

4.      (5 + 3) > (3 + 6)
        -- compiles

## Multiple choice

1. The `Eq` class
    * a. makes  equality tests possible

2. The typeclass `Ord`
    * b. is a subclass of `Eq`

3. Supponse the typeclass `Ord` has an operator `>`. What is the type of `>`?
    * a. `Ord a => a -> a -> Bool`

4. In `x = divMod 16 12`
    * b. the value of x is undecidable

5. The typeclass `Integral` includes 
    * a. `Int` and `Integer` numbers


## Does it typecheck?

1. No, `Person` type has no instance of `Show` class

2. No, `Mood` type has no instance of `Eq` class

3.
    * a. `Blah` and `Woot`
    * b. Type error, there is no way to compare a number with a Mood type value
    * c. Type error, `Mood` type has no instance for `Ord` class

4. Yes

## Given a datatype declaration, what can we do?

1. No, `Papu` takes `Rock` and `Yeah` datatype values as arguments, not `Bool` and `String`

2. Yes

3. No, `Papu` has no instance for `Eq`

4. No, `Papu` has no instance for `Ord`

## Match the types

1. No, it must have a `Num` constraint

2. No, it must have a `Fractional` constraint 

3. Yes

4. Yes

5. Yes

6. Yes

7. No. The function will always return a `Int` value, not a value of any type

8. No, there are other types that are also instances of `Num`, not only `Int`

9. Yes

10. Yes

11. No, `mySort` works only with lists of `Char`

## Type-Kwon-Do Two: Electric Typealoo

1.      chk :: Eq b => (a -> b) -> a -> b -> Bool
        chk f a = (==) (f a)

2.      arith :: Num b => (a -> b) -> Integer -> a -> b
        arith f n = (+) (f n)