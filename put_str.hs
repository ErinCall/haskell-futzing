put_str :: String -> IO ()
put_str "" = return ()
put_str (x:xs) = do
    putChar x
    put_str xs

main = do
    put_str "yeah hello"
