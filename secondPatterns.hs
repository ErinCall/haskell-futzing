main = do
    putStrLn $ show $ second [1, 2, 3, 4]

second []      = Nothing
second [_]     = Nothing
second (x:y:_) = Just y
