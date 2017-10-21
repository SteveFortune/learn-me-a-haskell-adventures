module Todo.Dispatch
( dispatch
, dispatchCmd
) where

import System.IO
import System.Environment
import Todo.Cmds

dispatch :: [(String, TodoCmd)]
dispatch = [ ("list", listTodosCmd)
           , ("add", addTodoCmd)
           ,  ("delete", deleteTodoCmd)
           ]

dispatchCmd :: IO ()
dispatchCmd = do
  (cmdName:args) <- getArgs
  let (Just action) = lookup cmdName dispatch
  action args
