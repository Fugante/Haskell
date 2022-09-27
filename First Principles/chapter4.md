# Chapter 4 excersises

## Mood Swing

```
data Mood = Blah | Woot derivin Show
```
1. Mood

2. Blah and Woot

3. Type signatures can only have type constructors, not data contructors

4. `Change Blah = Woot`.

5. 
        Data Mood = Blah | Woot deriving Show
        
        ChangeMood :: Mood -> Mood
        ChangeMood Blah = Woot
        ChangeMood _ = Blah


## Find the mistakes

1.      not True && True

2.      not (x == 6)

3.      (1 * 2) > 5

4.      ["Merry"] > ["Happy"]

5.      ['1', '2', '3'] ++ "look at me!"


## Chapter excercises

```
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]
```

1. Type signature: `[a] -> Integer`

2.  * a. `length [1, 2, 3, 4, 5] --> 5`
    * b. `length [(1,2),(2,3),(3,4)] --> 3`
    * c. `length allAwesome --> 2`
    * d. `length (concat allAwesome) --> 5`

3. `6 / length [1,2,3]` will fail since length returns an `Integer`

4. ```6 `div` length [1,2,3]```

5. type: `Bool`, result: `True`

6. type: `Bool`, result: `False`

7.  * `length allAwesome == 2`: works, evaluates to `True`
    * `length [1, 'a', 3, 'b']`: doesn't work, all members of a list must be of the same type
    * `length allAwesome + length awesome`: works, evaluates to `5` 
    * `(8 == 8) && 'b' < 'a'`: works, evaluates to `False`
    * `(8 == 8) && 9`: doesn't work, `Integral` has not instance of `Eq`

8.      isPalindrome :: Eq a => [a] -> Bool
        isPalindrome xs = xs == reverse xs

9.      myAbs :: Integer -> Integer
        myAbs n = if n < 0 then negate n else n

10.     f :: (a, b) -> (c, d) -> ((b, d), (a, c))
        f t u = ((snd t, snd u), (fst t, fst u))


### Correcting syntax

1.      x = (+)
        f xs = w `x` 1
            where w = length xs

2.      \x -> x

3.      f (a, b) = a


### Match the function names to their types

1. Which of the following types is the type of `show`?
    * c. `Show a => a -> String`

2. Which of the following types is the type of `(==)`?
    * b. `Eq a => a -> a -> Bool`

3. Which of the following types is the type of `fst`?
    * a. `(a, b) -> a`

4. Which of the following types is the type of `(+)`?
    * d. `Num a => a -> a -> a`