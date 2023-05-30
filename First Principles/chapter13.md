# Chapter 13 exercises

## Intermission: Check your understanding

```haskell
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as MV
import qualified Data.ByteString.Char8 as B
import qualified Data.Locator as DL
import qualified Data.Time.Clock.POSIX as PSX
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info as NI
import qualified Safe
import Control.Exception ( mask, try )
import Control.Monad ( forever, when )
import Data.Bits
import Data.Bits.Bitwise ( fromListBE )
import Data.List.Split ( chunksOf )
import Database.Blacktip.Types
import System.IO.Unsafe ( unsafePerformIO )
```

1. `forever` and `when`

2. `Data.Bits` and `Database.Blacktip.Types`

3. Blacktips data and type definitions

4.
    ```haskell
    writeTimestamp :: MV.MVar ServerState -> FPC.FilePath -> IO CC.ThreadId
    writeTimestamp s path = do
        CC.forkIO go
        where go = forever $ do
            ss <- MV.readMVar s
            mask $ \_ -> do
                FS.writeFile path
                (B.pack (show (ssTime ss)))
                -- sleep for 1 second
            CC.threadDelay 1000000
    ```
    a. `Control.Concurrent.MVar`, `Filesystem.Path.CurrentOS` and `Control.Concurrent`

    b. `import qualified Filesystem as FS`

    c. `import Control.Monad ( forever, when )`


## Chapter exercises

### Hangman game logic

1.
    ```haskell
    import Data.Char


    _ASCIIa = ord 'a'
    _NUMLETTERS = ord 'z' - _ASCIIa + 1

    caesar :: Int -> Char -> Char
    caesar shift letter = chr $ _ASCIIa + (ord letter + shift - _ASCIIa) `mod` _NUMLETTERS

    cipher :: Int -> String -> String
    cipher shift = map (caesar shift)

    main :: IO ()
    main = do
        putStr "Enter a number to shift the letters by: "
        shift <- getLine
        putStr "Enter a word to encrypt: "
        word <- getLine
        putStrLn $ cipher (read shift) word
    ```

2.
    ```haskell
    import Control.Monad
    import System.Exit (exitSuccess)


    palindrome :: IO()
    palindrome = forever $ do
        line1 <- getLine
        if line1 == reverse line1 then
            putStrLn "It's a palindrome!"
        else
            do
                putStrLn "Nope1"
                exitSuccess
    ```

3.
    ```haskell
    import Control.Monad
    import System.Exit (exitSuccess)
    import Data.Char (toLower, isAlpha)


    stripNonAscii :: String -> String
    stripNonAscii word = [toLower char | char <- word, isAlpha char]

    palindrome :: IO()
    palindrome = forever $ do
        rawInput <- getLine
        let word = stripNonAscii rawInput
        if word == reverse word then
            putStrLn "It's a palindrome!"
        else
            do
                putStrLn "Nope1"
                exitSuccess
    ```

4.
    ```haskell
    type Name = String
    type Age = Integer

    data Person = Person Name Age deriving Show

    data PersonInvalid =
        NameEmpty |
        AgeTooLow |
        PersonInvalidUnknown String
        deriving (Eq, Show)

    mkPerson:: Name -> Age -> Either PersonInvalid Person
    mkPerson name age
        | name /= "" && age > 0 = Right $ Person name age
        | name == "" = Left NameEmpty
        | not (age > 0) = Left AgeTooLow
        | otherwise = Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++ " Age was: " ++ show age

    gimmePerson :: IO ()
    gimmePerson = do
        putStr "Enter the person's name: "
        name <- getLine
        putStr "Enter the person's age: "
        ageInput <- getLine
        let
            age = read ageInput :: Age
            personAttempt = mkPerson name age
        case personAttempt of
            (Right person) -> putStrLn $ "Yay! Successfully got a person: " ++ show person
            (Left invalidPerson) -> putStrLn $ "An error ocurred: " ++ show invalidPerson
    ```