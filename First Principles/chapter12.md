# Chapter 12

## Chapter Exercises

### Determine the kinds

1. `id :: a -> a` has kind `* -> *`

2. `r :: a -> f a` has kind `* -> * -> *`


### String processing

1.
    ```haskell
    notThe :: String -> Maybe String
    notThe "the" = Nothing
    notThe word = Just word

    replaceThe :: String -> String
    replaceThe = unwords . replace . words
        where
            replace :: [String] -> [String]
            replace [] = []
            replace (w:ws) = case notThe w of
                Nothing -> "a" : replace(ws)
                Just w -> w : replace(ws)
    ```

2.
    ```haskell
    countTheBeforeVowel :: String -> Integer
    countTheBeforeVowel = count . words
        where
            count :: [String] -> Integer
            count [] = 0
            count (_:[]) = 0
            count (w1:w2:ws) = case (w1, startsWithVowel w2) of
                ("the", True) -> 1 + count ws
                _ -> count ws

            startsWithVowel :: String -> Bool
            startsWithVowel "" = False
            startsWithVowel (w:_) = elem w "aeiou"
    ```

3.
    ```haskell
    countVowels :: String -> Integer
    countVowels = foldr go 0
        where
            go :: Char -> Integer -> Integer
            go char count = if isVowel char then count + 1 else count

            isVowel :: Char -> Bool
            isVowel char = elem char "aeiou"
    ```