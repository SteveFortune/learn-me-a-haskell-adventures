import System.IO


mapTodo :: String -> String
mapTodo = (todoPrefix ++)
  where todoPrefix = "- "

mapTodoItems :: [String] -> [String]
mapTodoItems = map mapTodo

formatTodoContent :: String -> String
formatTodoContent = unlines . mapTodoItems . lines

main = do
  putStrLn welcomeMessage
  todoContent <- readFile fileName
  putStrLn . formatTodoContent $ todoContent
  where welcomeMessage =
          "Welcome to your todo list. The following " ++
          "are your todos:\n"
        fileName = "todo.txt"

