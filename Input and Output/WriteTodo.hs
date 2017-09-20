import System.IO

fileName = "todo.txt"
promptMessage = "Please enter a todo."
successMessage = "Your todo has been added."

main = do
  putStrLn promptMessage
  todo <- getLine
  appendFile fileName (todo ++ "\n")
  putStrLn successMessage
