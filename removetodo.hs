import System.IO
import System.Directory
import Data.List

main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    numberString <- getLine
    let index = read numberString
        stillTodo = unlines $ delete (todoTasks !! index) todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle stillTodo
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"

