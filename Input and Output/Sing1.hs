import Data.Char
import Control.Monad

-- `return` takes some value and wraps in an IO action.
-- We use it at the end of the `then` expression with the
-- empty tuple so that main is assigned to an empty IO action
-- Note: `return` does not affect execution flow!
main = do
  sentence <- getLine
  if null sentence
     then return () -- Has to return an IO action also !
     else do -- Note that this else expression has to be in a do block also because
             -- It handles IO actions
      putStrLn $ reverseWords sentence
      main -- Return main from main - recursively building up an IO action

