import Data.Char
-- Some pure code - used in an unpure do block
yellAtMe :: String -> String
yellAtMe name = "God damn it, " ++ name

-- do actions package up lots of IO ()s
-- <- gets the IO result from an IO () - its separated from the
-- pure parts of the code because its results can vary depending on
-- execution
-- do blocks are only performed once they are assigned to main
main = do
  putStrLn "Give me your first name"
  firstName <- getLine
  putStrLn "Give me your last name"
  lastName <- getLine
  -- Can use let bindings in do blocks!
  let sanatize fn = map fn . filter (not . isSpace)
      bigFirstName = sanatize toUpper firstName
      bigLastName = sanatize toLower lastName
      name = bigFirstName ++ " " ++ bigLastName
  putStrLn . yellAtMe $ name -- Cannot bind the result of the last op in the do block
