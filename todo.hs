import Control.Exception
import System.IO
import System.Environment
import System.Directory
import Data.List

main = do
    args <- getArgs
    let command = head args
        subArgs = tail args
    determineCommand command subArgs

determineCommand :: String -> [String] -> IO ()
determineCommand commandName
    | commandName == "add"    = addTodo
    | commandName == "view"   = viewTodos
    | commandName == "remove" = removeTodo
determineCommand commandName = \_ -> do
    putStrLn $ "I don't know how to " ++ commandName

addTodo :: [String] -> IO ()
addTodo args = do
    let filename = head args
        task     = args !! 1
    appendFile filename $ task ++ "\n"

viewTodos :: [String] -> IO ()
viewTodos args = do
    contents <- readFile $ head args
    let numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ lines contents
    putStr $ unlines numberedTasks

removeTodo :: [String] -> IO ()
removeTodo args = do
    let filename  = head args
        taskIndex = read $ args !! 1
    contents <- readFile filename
    let todoTasks = lines contents
        stillTodo = unlines $ delete (todoTasks !! taskIndex) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle stillTodo
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")

