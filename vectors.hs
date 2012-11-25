data Vector a = Vector a a a
  deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProd :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `dotProd` (Vector l m n) = Vector (i * l) (j * m) (k * n)

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` x = Vector (i * x) (j * x) (k * x)
