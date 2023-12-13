# Chapter 26 exercises

## Exercises: EitherT

```haskell
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
```

1.
    ```haskell
    instance Functor m => Functor (EitherT e m) where
        fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
        fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea
    ```

2.
    ```haskell
    instance Applicative m => Applicative (EitherT e m) where
        pure :: a -> EitherT e m a
        pure = EitherT . pure . Right

        (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
        f <*> a = EitherT $ (<*>) <$> runEitherT f <*> runEitherT a
    ```

3.
    ```haskell
    instance Monad m => Monad (EitherT e m) where
        return :: a -> EitherT e m a
        return = pure

        (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
        v >>= f = EitherT $ runEitherT v >>= go
            where
                go (Left e) = return $ Left e
                go (Right a) = runEitherT $ f a
    ```

4.
    ```haskell
    swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
    swapEitherT mea = EitherT $ go <$> runEitherT mea
        where
            go :: Either e a -> Either a e
            go (Left e) = Right e
            go (Right a) = Left a
    ```

5.
    ```haskell
    eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
    eitherT f g (EitherT mab) = mab >>= go
        where
            go (Left a) = f a
            go (Right b) = g b
    ```

## ReaderT

```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f f' = ReaderT $ (fmap . fmap) f (runReaderT f')

instance Applicative m => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure a = ReaderT $ pure (pure a)
    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    f <*> a = ReaderT $ (<*>) <$> runReaderT f <*> runReaderT a

instance Monad m => Monad (ReaderT r m) where
    return :: a -> ReaderT r m a
    return = pure
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    ReaderT rma >>= f = ReaderT $ \r -> rma r >>= \a -> runReaderT (f a) r
```

## Exercises: StateT

```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
```

1.
    ```haskell
    instance (Functor m) => Functor (StateT s m) where
        fmap :: (a -> b) -> StateT s m a -> StateT s m b
        fmap f m = StateT $ fmap go . runStateT m
            where go (a, s) = (f a, s)
    ```

2.
    ```haskell
    instance (Monad m) => Applicative (StateT s m) where
        pure :: a -> StateT s m a
        pure a = StateT $ \s -> pure (a, s)

        (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
        f <*> sma = StateT $ \s -> do
            (f', s1) <- runStateT f s
            (a, s2) <- runStateT sma s1
            return (f' a, s2)
    ```

3.
    ```haskell
    instance (Monad m) => Monad (StateT s m) where
        return :: a -> StateT s m a
        return = pure

        (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
        sma >>= f = StateT $ \s -> runStateT sma s >>= \(a, s1) -> runStateT (f a) s1
        -- sma >>= f = StateT $ \s -> do
        --     (a, s1) <- runStateT sma s
        --     runStateT (f a) s1
    ```

## Exercise: Wrap It Up

```haskell
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT $ return <$> const (Right (Just 1))
```

## MonadTrans

3.
    ```haskell
    instance MonadTrans (ReaderT r) where
        lift :: Monad m => m a -> ReaderT r m a
        lift = ReaderT . const
    ```

### Exercises: Lift More

1.
    ```haskell
    instance MonadTrans (EitherT e) where
        lift :: Monad m => m a -> EitherT e m a
        lift = EitherT . fmap Right
    ```

2.
    ```haskell
    instance MonadTrans (StateT s) where
        lift :: Monad m => m a -> StateT s m a
        lift ma = StateT $ \s -> (,) <$> ma <*> pure s
        -- lift ma = StateT $ \s -> ma >>= \a -> return (a, s)
    ```


## Exercises: Some Instances

1.
    ```haskell
    instance MonadIO m => MonadIO (MaybeT m) where
        liftIO :: MonadIO m => IO a -> MaybeT m a
        liftIO = lift . liftIO
    ```

2.
    ```haskell
    instance MonadIO m => MonadIO (ReaderT r m) where
        liftIO :: MonadIO m => IO a -> ReaderT r m a
        liftIO = lift . liftIO
    ```

3.
    ```haskell
    instance MonadIO m => MonadIO (StateT s m) where
        liftIO :: IO a -> StateT s m a
        liftIO = lift . liftIO
    ```

## Chapter Exercises

### Write the code
```haskell
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity
```

1.
    ```haskell
    rDec :: Num a => Reader a a
    rDec =  ReaderT $ \a -> pure $ subtract 1 a
    ```

2.
    ```haskell
    rDec :: Num a => Reader a a
    rDec =  ReaderT $ pure . subtract 1
    ```

3.
    ```haskell
    rShow :: Show a => ReaderT a Identity String
    rShow = ReaderT $ \a -> Identity $ show a
    ```

4.
    ```haskell
    rShow :: Show a => ReaderT a Identity String
    rShow = ReaderT $ Identity . show
    ```

5.
    ```haskell
    rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
    rPrintAndInc = ReaderT $ \a -> putStrLn ("Hi: " ++ show a) >> return (a + 1)
    ```

6.
    ```haskell
    sPrintIncAccum :: (Num a, Show a) => StateT a IO String
    sPrintIncAccum = StateT go
        where
            go :: (Num a, Show a) => a -> IO (String, a)
            go a = do
                let sa = show a
                putStrLn ("Hi: " ++ sa)
                return (sa, a + 1)
    ```

### Morra

```haskell
module Main (main) where

import Control.Monad.Trans.State
import Data.List
import System.Random
import Text.Read ( readMaybe )


data Parity = Even | Odd deriving (Eq, Show)

opposite :: Parity -> Parity
opposite Even = Odd
opposite Odd = Even

type Points = Int

type Fingers = Int

type Name = String

data PlayerType = Human | Computer deriving (Eq, Show)

data Player =
    Player
    { getType :: PlayerType
    , getName :: String
    , getParity :: Parity
    , getPoints :: Points }
    deriving (Eq, Show)

type Scoreboard = StateT [Player] IO [Points]

addPoints :: Player -> Points -> Player
addPoints (Player t n pa po) po' = Player t n pa (po + po')

numToParity :: Fingers -> Parity
numToParity n
    | even n = Even
    | otherwise = Odd

genFingers :: IO Fingers
genFingers = getStdRandom (uniformR (0, 5))

playerTurn :: Fingers -> Player -> Points
playerTurn f p
    | getParity p == numToParity f = 1
    | otherwise = 0

showPoints :: ([Points], [Player]) -> String
showPoints (points, players) = intercalate "  " $ zipWith go points players
    where
        go :: Points -> Player -> String
        go point player = getName player ++ ": " ++ show point

showScores :: [Player] -> String
showScores = foldr go ""
    where
        go :: Player -> String -> String
        go p s = s ++ "  " ++ getName p ++ ": " ++ show (getPoints p)

chooseParity :: IO Parity
chooseParity = do
    putStrLn "Choose even (e) or odd (o)"
    s <- getLine
    maybe chooseParity return (parse s)
        where
            parse "e" = Just Even
            parse "o" = Just Odd
            parse _ = Nothing

playHand :: IO Fingers
playHand = do
    putStrLn "Enter a number between 0 and 5"
    l <- getLine
    let m = readMaybe l :: Maybe Fingers
        in case m of
            Nothing -> playHand
            Just n -> if n < 0 || n > 5 then playHand else return n

getHands :: [Player] -> IO Fingers
getHands = foldr go (return 0)
    where
        go :: Player -> IO Fingers -> IO Fingers
        go (Player Human _ _ _) f = (+) <$> f <*> playHand
        go (Player Computer _ _ _) f = (+) <$> f <*> genFingers

playTurn :: [Player] -> IO ([Points], [Player])
playTurn players = do
    totalFingers <- getHands players
    let scores = playerTurn totalFingers <$> players
    return (scores, zipWith addPoints players scores)

game :: Scoreboard -> [Player] -> IO ()
game scoreboard players = do
    (points, players') <- runStateT scoreboard players
    putStrLn $ showPoints (points, players')
    putStrLn "Press enter to cotinue or 'q' to quit."
    r <- getLine
    case r of
        "q" -> putStrLn $ showScores players'
        _ -> game scoreboard players'

main :: IO ()
main = do
    p <- chooseParity
    let p1 = Player Human "Player" p 0
        p2 = Player Computer "Computer" (opposite p) 0
        scoreboard = StateT playTurn
    game scoreboard [p1, p2]
```