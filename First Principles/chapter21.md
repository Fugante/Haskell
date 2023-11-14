# Chapter 21 exercises

## Chapter Exercises

### Traversable instances

```haskell
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
    foldr :: (a -> b -> b) -> b -> Identity a -> b
    foldr f b (Identity a) = f a b

instance Traversable Identity where
    traverse :: Applicative f => (a -> f b) -> Identity a -> f (Identity b)
    traverse g (Identity a) = Identity <$> g a


newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where
    fmap :: (b -> c) -> Constant a b -> Constant a c
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldMap :: Monoid m => (b -> m) -> Constant a b -> m
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    sequenceA :: Applicative f => Constant a (f b) -> f (Constant a b)
    sequenceA (Constant a) = pure $ Constant a


data Optional a = Nada | Yep a

instance Functor Optional where
    fmap :: (a -> b) -> Optional a -> Optional b
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldr :: (a -> b -> b) -> b -> Optional a -> b
    foldr _ b Nada = b
    foldr f b (Yep a) = f a b

instance Traversable Optional where
    traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
    traverse _ Nada = pure Nada
    traverse g (Yep a) = Yep <$> g a


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
    (<>) :: List a -> List a -> List a
    Nil <> Nil = Nil
    Nil <> l = l
    l <> Nil = l
    (Cons a as) <> (Cons a' as') = Cons a (as <> Cons a' as')

instance Monoid (List a) where
    mempty :: List a
    mempty = Nil

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
    pure :: a -> List a
    pure a = Cons a Nil

    (<*>) :: List (a -> b) -> List a -> List b
    Cons f fs <*> Cons a as = Cons (f a) (f <$> as) <> (fs <*> Cons a as)
    _ <*> _ = Nil

instance Foldable List where
    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr _ b Nil = b
    foldr f b (Cons a as) = f a (foldr f b as)

instance Traversable List where
    -- traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
    -- traverse g = foldr go (pure Nil)
    --     where
    --         go a acc = ((<>) <$> (pure <$> g a)) <*> acc
    traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
    traverse  _ Nil = pure Nil
    traverse g (Cons a as) = Cons <$> g a <*> traverse g as


data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap :: (c -> d) -> Three a b c -> Three a b d
    fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
    foldr :: (c -> d -> d) -> d -> Three a b c -> d
    foldr f d (Three a b c) = f c d

instance Traversable (Three a b) where
    traverse :: Applicative f => (c -> f d) -> Three a b c -> f (Three a b d)
    traverse f (Three a b c) = Three a b <$> f c


data Pair a b = Pair a b

instance Functor (Pair a) where
    fmap :: (b -> c) -> Pair a b -> Pair a c
    fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
    foldr :: (b -> c -> c) -> c -> Pair a b -> c
    foldr f c (Pair a b) = f b c

instance Traversable (Pair a) where
    traverse :: Applicative f => (b -> f c) -> Pair a b -> f (Pair a c)
    traverse f (Pair a b) = Pair a <$> f b


data Big a b = Big a b b

instance Functor (Big a) where
    fmap :: (b -> c) -> Big a b -> Big a c
    fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
    foldMap :: Monoid m => (b -> m) -> Big a b -> m
    foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
    traverse :: Applicative f => (b -> f c) -> Big a b -> f (Big a c)
    traverse f (Big a b b') = Big a <$> f b <*> f b'


data Bigger a b = Bigger a b b b

instance Functor (Bigger a) where
    fmap :: (b -> c) -> Bigger a b -> Bigger a c
    fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
    foldMap :: Monoid m => (b -> m) -> Bigger a b -> m
    foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
    traverse :: Applicative f => (b -> f c) -> Bigger a b -> f (Bigger a c)
    traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''


data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap :: (a -> b) -> S n a -> S n b
    fmap f (S n a) = S (f <$> n) (f a)

instance Foldable n => Foldable (S n) where
    foldMap :: Monoid m => (a -> m) -> S n a -> m
    foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
    traverse :: Applicative f => (a -> f b) -> S n a -> f (S n b)
    traverse g (S n a) = S <$> traverse g n <*> g a
```

### Instances for tree

```haskell
data Tree a =
      Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node l a r) = f a <> foldMap f l <> foldMap f r

    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ b Empty = b
    foldr f b (Leaf a) = f a b
    foldr f b (Node l a r) = foldr f (foldr f (f a b) l) r

instance Traversable Tree where
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse _ Empty = pure Empty
    traverse g (Leaf a) = Leaf <$> g a
    traverse g (Node l a r) = Node <$> traverse g l <*> g a <*> traverse g r
```