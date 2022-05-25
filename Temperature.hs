module Temperature (tempToC, tempToF) where


tempToC :: Int -> Float
tempToC temp = (fromIntegral temp - 32) / 1.8

tempToF :: Float -> Int
tempToF temp = ceiling(temp * 1.8 + 32)