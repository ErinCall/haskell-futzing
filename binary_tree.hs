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
