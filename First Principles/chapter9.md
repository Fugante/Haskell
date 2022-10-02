# Chapter 9 excercises

## Excercise: EnumFromTo

```
eftBool :: Bool -> Bool -> [Bool]
eftBool b1 b2 = go b1 b2 []
    where go b0 bf c
            | bf == b0 = bf : c
            | bf < b0 = c
            | otherwise = go b0 (pred bf) (bf : c)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd ord1 ord2 = go ord1 ord2 []
    where go ord0 ordf c
            | ordf == ord0 = ordf : c
            | ordf < ord0 = c
            | otherwise = go ord0 (pred ordf) (ordf : c)

eftInt :: Int -> Int -> [Int]
eftInt n m = go n m []
    where go int0 intf c
            | intf == int0 = intf : c
            | intf < int0 = c
            | otherwise = go int0 (pred intf) (intf : c)

eftChar :: Char -> Char -> [Char]
eftChar a b = go a b []
    where go char0 charf c
            | charf == char0 = charf : c
            | charf < char0 = c
            | otherwise = go char0 (pred charf) (charf : c)
```