main = do
    putStrLn $ show $ second [1, 2, 3, 4]

second xs = if null xs || null (tail xs)
            then Nothing
            else Just (head (tail xs))
