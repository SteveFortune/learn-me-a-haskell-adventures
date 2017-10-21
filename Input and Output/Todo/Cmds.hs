module Todo.Cmds
( TodoCmd
, listTodosCmd
, addTodoCmd
, deleteTodoCmd
) where

import System.IO
import Todo.IO

type TodoCmd = [String] -> IO ()

loadTodosWithWelcome :: IO TodoList
loadTodosWithWelcome = do
  putStrLn welcomeMessage
  readAndPutTodos
  where
    welcomeMessage =
      "Welcome to your todo list. The following " ++
      "are your todos:\n"

runTodoCmd :: String -> String -> (TodoList -> IO ()) -> IO ()
runTodoCmd promptMessage successMessage cmdFn = do
  todos <- loadTodosWithWelcome
  putStrLn promptMessage
  cmdFn todos
  putStrLn successMessage
  readAndPutTodos
  return ()

listTodosCmd :: TodoCmd
listTodosCmd [] = do
  loadTodosWithWelcome
  return ()

addTodoCmd :: TodoCmd
addTodoCmd [newTodo] =
  runTodoCmd promptMessage successMessage (\_ ->
    appendTodo newTodo)
  where
    promptMessage = "Please enter a new todo."
    successMessage = "Your todo has been added:"

deleteTodoCmd :: TodoCmd
deleteTodoCmd [todoNumber] =
  runTodoCmd promptMessage successMessage (\todos ->
    deleteTodo todos $ read todoNumber)
  where
    promptMessage =
      "Please enter the number of the todo " ++
      "that you wish to delete:"
    successMessage = "Your todo has been deleted:"

