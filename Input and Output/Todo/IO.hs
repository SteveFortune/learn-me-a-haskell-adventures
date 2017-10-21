module Todo.IO (
  TodoList,
  readTodos,
  putTodos,
  appendTodo,
  deleteTodo,
) where

import Data.List
import System.IO
import System.Directory

todoFilePath = "todos.txt"
listNumberOffset = 1

type TodoList = [String]

formatTodos :: TodoList -> [String]
formatTodos [] = [emptyListMessage]
  where emptyListMessage = "No todos"
formatTodos todos = zipWith formatTodo [listNumberOffset..] todos
  where
    formatTodo :: Int -> String -> String
    formatTodo n todo =
      let todoPrefix = (show n) ++ todoSep
      in todoPrefix ++ todo
    todoSep = ". "

readTodos :: IO TodoList
readTodos = do
  fileContent <- readFile todoFilePath
  return (lines fileContent)

putTodos :: TodoList -> IO ()
putTodos todos = do
  mapM putStrLn $ formatTodos todos
  return ()

appendTodo :: String -> IO ()
appendTodo todo = appendFile todoFilePath (todo ++ "\n")

deleteTodo :: TodoList -> Int -> IO ()
deleteTodo todos number = do
  (tempName, tempHandle) <- openTempFile currentDir tempFileExt
  let newTodos = delete (todos !! todoIndex) todos
      todoIndex = number - listNumberOffset
  hPutStr tempHandle $ unlines newTodos
  hClose tempHandle
  removeFile todoFilePath
  renameFile tempName todoFilePath
  where
    currentDir = "."
    tempFileExt = "temp"
