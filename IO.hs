
-- do actions package up lots of IO ()s
-- <- gets the IO result from an IO () - its separated from the
-- pure parts of the code because its results can vary depending on
-- execution
-- do blocks are only performed once they are assigned to main
main = do
  putStrLn "Hello, world"
  name <- getLine
  putStrLn ("Hello, " ++ name) -- Cannot bind the result of the last op in the do block
