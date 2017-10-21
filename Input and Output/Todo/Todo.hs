module Todo
( filePath
, getTodos
, putTodos
) where

import System.IO

filePath = "todos.txt"

type TodoList TodoList

--
--
formatTodoContent :: TodoList -> TodoList
formatTodoContent = unlines . formatTodos . lines
  where formatTodos :: TodoList -> TodoList
        formatTodos = zipWith (\n todo) formatTodo
        formatTodo :: (Integral a) => a -> String -> String
        formatTodo n = (todoPrefix ++)
        todoPrefix = n ++ todoSeparator
        todoSeparator = ". "

parseTodoItems :: String -> TodoList
parseTodoItems = formatTodoItems . lines

formatTodoContent :: TodoList -> String
formatTodoContent = unlines

getTodos :: IO TodoList
getTodos = do
  fileContent <- readFile filePath
  return (parseTodoItems fileContent)

putTodos :: IO ()
putTodos = do
  todoContent <- getTodos
  putStrLn . formatTodoContent $ todoContent
