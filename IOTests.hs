import Data.Char
import Control.Monad

-- `return` takes some value and wraps in an IO action.
-- We use it at the end of the `then` expression with the
-- empty tuple so that main is assigned to an empty IO action
-- Note: `return` does not affect execution flow!
main1 = do
  sentence <- getLine
  if null sentence
     then return () -- Has to return an IO action also !
     else do -- Note that this else expression has to be in a do block also because
             -- It handles IO actions
      putStrLn $ reverseWords sentence
      main1 -- Return main from main - recursively building up an IO action

-- getChar is buffered, so as the user types the chars will be
-- held until they press enter and the program will execute!
main2 = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    main2

-- forever takes an IO action and returns an IO action that calls
-- itself endlessly
main3 = forever (do
  putStrLn "Gis a line, bruv"
  line <- getLine
  putStrLn $ map toUpper line)

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

mapPalindrome :: String -> String
mapPalindrome str = if isPal str then "Palindrome!" else "Not palindrome"
  where isPal str = str == reverse str

interactLns :: (String -> String) -> IO ()
interactLns fn = interact $ unlines . (map fn) . lines

main =  interactLns mapPalindrome

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
