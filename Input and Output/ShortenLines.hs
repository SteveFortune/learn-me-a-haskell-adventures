import Data.Char
import Control.Monad
import IO

-- The subtle bit here is that the `lines` fn used in `shortLines
-- is the bit which prompts the result of `getContents` to actually
-- return a lines from stdin
main = do
  -- Could have just done `interact shortLines` - encapsulates the
  -- function of reading something from stdin, transforming it and
  -- then printing it
  input <- getContents
  putStr $ shortLinesOnly input

shortLinesOnly :: String -> String
shortLinesOnly str =
  let allLines = lines str
      shortLines = filter ((< 10) . length) allLines
  in unlines shortLines

-- One liner!
-- main = interact $ unlines . filter ((< 10) . length) . lines
