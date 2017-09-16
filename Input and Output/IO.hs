module IO
( reverseWords
, putStr'
) where

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- Put string is recursively defined with putChar - the edge condition
-- is an empty string, in which case it just returns a noop action, else
-- it recursively builds an IO action from putChar and putStr
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x
  putStr' xs

-- print is effectively: putStrLn . show
-- sequence is of type [IO a] -> IO [a], meaning it takes a list of
-- IO actions and returns an IO action containing a list with the
-- results of all other IO actions in it.

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' fn = sequence . map fn
