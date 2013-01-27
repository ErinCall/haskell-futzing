instance Applicative Maybe where
    pure = Just
    `<*>` Nothing  _      = Nothing
    `<*>` (Just f) Just x = fmap f x
