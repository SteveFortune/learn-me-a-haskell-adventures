import System.IO
import Todo

main = do
  putStrLn promptMessage
  todo <- getLine
  appendFile Todo.filePath (todo ++ "\n")
  putStrLn successMessage
  where promptMessage = "Please enter a todo."
        successMessage = "Your todo has been added."


