import Data.Char
import Control.Monad

mapPalindrome :: String -> String
mapPalindrome str = if isPal str then "Palindrome!" else "Not palindrome"
  where isPal str = str == reverse str

interactLns :: (String -> String) -> IO ()
interactLns fn = interact $ unlines . (map fn) . lines

main =  interactLns mapPalindrome
