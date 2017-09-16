import Data.Char
import System.IO

-- main = do
--  handle <- openFile "lyrics.txt" ReadMode
--  contents <- hGetContents handle
--  putStr contents
--  hClose handle

-- main = withFile "lyrics.txt" ReadMode (\handle -> do
--  contents <- hGetContents handle
--  putStr contents)

main = do
  contents <- readFile "lyrics.txt"
  writeFile "lyrics_upper.txt" $ map toUpper contents
