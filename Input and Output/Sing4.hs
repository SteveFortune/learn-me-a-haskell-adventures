import Data.Char
import Control.Monad

--  main = do
--    colours <- sequence (map (\a -> do
--      putStrLn $ "What colour do you associate with " ++ show a ++ "?"
--      getLine) [1..10])
--    putStrLn "The colours you associated with 1 - 10 are:"
--    sequence (map putStrLn colours)

main4 = do
  colours <- forM [1..10] (\a -> do
    putStrLn $ "What colour do you associate with " ++ show a ++ "?"
    getLine)
  putStrLn "The colours you associated with 1 - 10 are:"
  mapM putStrLn colours

