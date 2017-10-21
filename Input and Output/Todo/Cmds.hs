module Todo.Cmds (
  myTodosCmd,
  addTodoCmd,
  deleteTodoCmd,
) where

import Todo.IO
import System.IO


loadMyTodos :: IO (TodoList)
loadMyTodos = do
  todos <- readTodos
  putTodos todos
  return todos

loadMyTodosWithWelcome :: IO (TodoList)
loadMyTodosWithWelcome = do
  putStrLn welcomeMessage
  loadMyTodos
  where
    welcomeMessage =
      "Welcome to your todo list. The following " ++
      "are your todos:\n"

runTodoCmd :: String -> String -> (TodoList -> IO ()) -> IO ()
runTodoCmd promptMessage successMessage cmdFn = do
  todos <- loadMyTodosWithWelcome
  putStrLn promptMessage
  cmdFn todos
  putStrLn successMessage
  loadMyTodos
  return ()

myTodosCmd :: IO ()
myTodosCmd = do
  loadMyTodosWithWelcome
  return ()

addTodoCmd :: IO ()
addTodoCmd =
  runTodoCmd promptMessage successMessage (\todos -> do
    newTodo <- getLine
    appendTodo newTodo)
  where
    promptMessage = "Please enter a new todo."
    successMessage = "Your todo has been added:"

deleteTodoCmd :: IO ()
deleteTodoCmd =
  runTodoCmd promptMessage successMessage (\todos -> do
    todoNumber <- getLine
    deleteTodo todos $ read todoNumber)
  where
    promptMessage =
      "Please enter the number of the todo " ++
      "that you wish to delete:"
    successMessage = "Your todo has been deleted:"

