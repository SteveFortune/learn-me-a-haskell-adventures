module Todo.IO
( TodoList
, readTodos
, putTodos
, readAndPutTodos
, appendTodo
, deleteTodo
) where

import System.IO
import System.Directory
import Data.List

todoFilePath = "todos.txt"
listIndxBase = 1

type TodoList = [String]

formatTodos :: TodoList -> [String]
formatTodos [] = [emptyMessage]
  where emptyMessage  = "No todos"
formatTodos todos = zipWith formatTodo [listIndxBase..] todos
  where
    formatTodo :: Int -> String -> String
    formatTodo n todo =
      let todoSep = ". "
          todoPrefix = (show n) ++ todoSep
      in todoPrefix ++ todo

readTodos :: IO TodoList
readTodos = do
  fileContent <- readFile todoFilePath
  return (lines fileContent)

putTodos :: TodoList -> IO ()
putTodos todos = do
  mapM putStrLn $ formatTodos todos
  return ()

readAndPutTodos :: IO TodoList
readAndPutTodos = do
  todos <- readTodos
  putTodos todos
  return todos

appendTodo :: String -> IO ()
appendTodo todo = appendFile todoFilePath (todo ++ "\n")

deleteTodo :: TodoList -> Int -> IO ()
deleteTodo todos number = do
  let newTodos = delete (todos !! todoIndex) todos
      todoIndex = number - listIndxBase
  (tempName, tempHandle) <- openTempFile currentDir tempFileExt
  hPutStr tempHandle $ unlines newTodos
  hClose tempHandle
  removeFile todoFilePath
  renameFile tempName todoFilePath
  where
    currentDir = "."
    tempFileExt = "temp"
