import Control.Exception
import System.IO
import System.Environment
import System.Directory
import Data.List

main = do
    (command:args) <- getArgs
    determineCommand command args

determineCommand :: String -> [String] -> IO ()
determineCommand commandName
    | commandName == "add"    = addTodo
    | commandName == "view"   = viewTodos
    | commandName == "remove" = removeTodo
determineCommand commandName = \_ -> do
    putStrLn $ "I don't know how to " ++ commandName

addTodo :: [String] -> IO ()
addTodo [filename, task] = do
    appendFile filename $ task ++ "\n"

viewTodos :: [String] -> IO ()
viewTodos [filename] = do
    contents <- readFile filename
    let numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ lines contents
    putStr $ unlines numberedTasks

removeTodo :: [String] -> IO ()
removeTodo [filename, taskIndex] = do
    contents <- readFile filename
    let todoTasks = lines contents
        stillTodo = unlines $ delete (todoTasks !! read taskIndex) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle stillTodo
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")

