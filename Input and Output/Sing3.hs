import Data.Char
import Control.Monad

-- forever takes an IO action and returns an IO action that calls
-- itself endlessly
main = forever (do
  putStrLn "Gis a line, bruv"
  line <- getLine
  putStrLn $ map toUpper line)

