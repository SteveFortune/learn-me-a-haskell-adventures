import Data.Char
import Control.Monad

-- The subtle bit here is that the `lines` fn used in `shortLines
-- is the bit which prompts the result of `getContents` to actually
-- return a lines from stdin
main5 = do
  -- Could have just done `interact shortLines` - encapsulates the
  -- function of reading something from stdin, transforming it and
  -- then printing it
  input <- getContents
  let shortLines :: String -> String
      shortLines str = unlines shortLns
        where allLns = lines str
              shortLns = filter ((< 10) . length) allLns
  putStr $ shortLines input
-- One-liner: `main = interact $ unlines . filter((<10) . length) . lines`

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
