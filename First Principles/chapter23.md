# Chapter 23 exercises

## Exercises: Roll Your Own

1.
    ```haskell
    rollsToGetN :: Int -> StdGen -> Int
    rollsToGetN n = go 0 0
        where
            go :: Int -> Int -> StdGen -> Int
            go sum count gen
                | sum >= n = count
                | otherwise =
                    let (die, nextGen) = randomR (1, 6) gen
                    in go (sum + die) (count + 1) nextGen
    ```

2.
    ```haskell
    rollsCountLogged :: Int -> StdGen -> (Int, [Die])
    rollsCountLogged n = go 0 (0, [])
        where
            go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
            go sum (count, dice) gen
                | sum >= n = (count, dice)
                | otherwise =
                    let (die, nextGen) = randomR (1, 6) gen
                    in go (sum + die) (count + 1, intToDie die : dice) nextGen
    ```

## Write State for yourself

```haskell
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $
        \s ->
            let (a, s') = g s
            in (f a, s')

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ (,) a

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    Moi f <*> Moi g = Moi $
        \s ->
            let (f', s') = f s
                (a, s'') = g s'
            in (f' a, s'')

instance Monad (Moi s) where
    return :: a -> Moi s a
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    Moi f >>= g = Moi $
        \s ->
            let (a, s') = f s
                Moi h = g a
            in h s'
```

## Fizzbuzz Differently

```haskell
fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo a b = fizzbuzzList [b, b - 1 ..a]
```

## Chapter exercises

```haskell
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) = State $
        \s ->
            let (a, s') = g s
            in (f a, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ (,) a

    (<*>) :: State s (a -> b) -> State s a -> State s b
    State f <*> State g = State $
        \s ->
            let (f', s') = f s
                (a, s'') = g s'
            in (f' a, s'')

instance Monad (State s) where
    return :: a -> State s a
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    State f >>= g = State $
        \s ->
            let (a, s') = f s
                State h = g a
            in h s'
```
1.
    ```haskell
    get :: State s s
    get = State $ \s -> (s, s)
    ```

2.
    ```haskell
    put :: s -> State s ()
    put s = State $ const ((), s)
    ```

3.
    ```haskell
    exec :: State s a -> s -> s
    exec s = snd . runState s
    ```

4.
    ```haskell
    eval :: State s a -> s -> a
    eval (State sa) = fst . sa
    ```

5.
    ```haskell
    modify :: (s -> s) -> State s ()
    modify f = State $ \s -> ((), f s)
    ```