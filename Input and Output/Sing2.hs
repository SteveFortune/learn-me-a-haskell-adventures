import Data.Char
import Control.Monad

-- getChar is buffered, so as the user types the chars will be
-- held until they press enter and the program will execute!
main = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    main2

