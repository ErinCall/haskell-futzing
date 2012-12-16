data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insert :: (Ord a) => a -> (Tree a) -> (Tree a)
insert x EmptyTree = singleton x
insert x (Node val left right)
    | x == val = Node val left            right
    | x <  val = Node val (insert x left) right
    | x >  val = Node val left            (insert x right)

inTree :: (Ord a) => a -> (Tree a) -> Bool
inTree _ EmptyTree = False
inTree x (Node val left right)
    | x == val = True
    | x <  val = inTree x left
    | x >  val = inTree x right

data Assoc a b = Assoc a b
    deriving (Show)

instance (Ord a) => Ord (Assoc a b) where
    (>) (Assoc x _) (Assoc y _) = x > y
    (<) (Assoc x _) (Assoc y _) = x < y

instance (Eq a) => Eq (Assoc a b) where
    (==) (Assoc x _) (Assoc y _) = x == y

find :: (Ord a) => a -> Tree (Assoc a b) -> Maybe b
find _ EmptyTree = Nothing
find x (Node (Assoc key val) left right)
    | x == key = Just val
    | x <  key = find x left
    | x >  key = find x right
