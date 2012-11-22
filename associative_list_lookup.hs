findKey :: (Eq k) => [(k, v)] -> k -> Maybe v
findKey []         _   = Nothing
findKey ((k,v):xs) key
  | key == k  = Just v
  | otherwise = findKey xs key
