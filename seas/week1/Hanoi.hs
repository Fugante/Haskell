type Peg = String
type Move = (Peg, Peg)
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a,c)] ++ hanoi (n - 1) b a c