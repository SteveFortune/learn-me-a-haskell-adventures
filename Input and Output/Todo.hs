module Todo
( filePath
, getTodos
, putTodos
) where

import System.IO

filePath = "todos.txt"

mapTodo :: String -> String
mapTodo = (todoPrefix ++)
  where todoPrefix = "- "

mapTodoItems :: [String] -> [String]
mapTodoItems [] = ["(None)"]
mapTodoItems xs = map mapTodo xs

parseTodoItems :: String -> [String]
parseTodoItems = mapTodoItems . lines

formatTodoContent :: [String] -> String
formatTodoContent = unlines

getTodos :: IO [String]
getTodos = do
  fileContent <- readFile filePath
  return (parseTodoItems fileContent)

putTodos :: IO ()
putTodos = do
  todoContent <- getTodos
  putStrLn . formatTodoContent $ todoContent
