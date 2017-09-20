import System.IO
import Todo

main = do
  putStrLn welcomeMessage
  putTodos
    where welcomeMessage = "Welcome to your todo list. The following " ++
            "are your todos:\n"

