# Chapter 11 exercises

## Exercises: Dog Types

```haskell
data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)
```

1. Type constructor

2. `* -> *`

3. `*`

4. `Num a => Doggies a`

5. `Integer a => Doggies Integer`

6. `Doggies String`

7. Both

8. `doge -> DoggeDeBordeaux doge`

9. `DogueDeBordeaux String`


## Exercises: Vehicles

```haskell
data Price = Price Integer deriving(Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving(Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving(Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline deriving(Eq, Show)


myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAi
```

1. `Vehicle`

2.
    ```haskell
    isCar :: Vehicle -> Bool
    isCar (Car _ _) = True
    isCar _ = False

    isPlane :: Vehicle -> Bool
    isPlane (Plane _) = True
    isPlane _ = False

    areCars :: [Vehicle] -> [Bool]
    areCars = map isCar
    ```

3.
    ```haskell
    getManu :: Vehicle -> Manufacturer
    getManu (Car manufacturer _) = manufacturer
    ```

4. It will raise an exception.

5.
    ```haskell
    data Size = Size Integer deriving (Eq, Show)

    data Vehicle' = Car' Manufacturer Price | Plane' Airline Size deriving(Eq, Show)

    isPlane' :: Vehicle' -> Bool
    isPlane' (Plane' _ _) = True
    isPlane' _ = False
    ```