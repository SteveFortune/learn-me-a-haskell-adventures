import System.IO
import Data.List
import Todo

main = do
  putStrLn welcomeMessage
  todoItem <- getLine
  let todoItems = getTodos
  if length todoItems < todoNo
     then putStrLn indexErrorMessage
     else let newTodos = dropTodoAt (todoNo - 1) todoItems
           in writeFile Todo.filePath (unlines newTodos)
  putStrLn doneMessage
  putTodos
    where
          welcomeMessage = "Type the no of the todo item you wish to delete."
          doneMessage = "Here is your current todo list:\n"
          indexErrorMessage = "This is not a valid todo number."
