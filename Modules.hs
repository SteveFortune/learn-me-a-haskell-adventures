import Data.List (nub, sort, intersperse, intercalate)
-- import Data.List hiding (nub)
import qualified Data.Map as M
-- M.filter, M.null, etc

numUniques :: Eq a => [a] -> Int
numUniques = length . nub

-- intersperse 

dotMe :: String -> String
dotMe = intersperse '.'

-- intercalate 

spaceMe :: [String] -> String 
spaceMe = intercalate " "

-- transpose 
-- concat
-- and, or
-- any, all
-- iterate
-- takeWhile, dropWhile 

dateStockExceeded :: (Floating a, Ord a) => [(a, Int, Int, Int)] -> a -> (Int, Int, Int)
dateStockExceeded [] _ = error "Never"
dateStockExceeded stocks n = (year, month, day)
  where (_, year, month, day) = head $ dropWhile (\(val, _, _, _) -> val > n) stocks

splitByFirstSpace :: String -> (String, String)
splitByFirstSpace = span (/= ' ')
