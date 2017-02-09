import Data.List (nub, sort, intersperse, intercalate, group, sort, tails, find)
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

dateStockExceeded :: (Floating a, Ord a) => a -> [(a, Int, Int, Int)] -> Maybe (a, Int, Int, Int)
-- This implementation is not safe, as if no stock is found that matches
-- n we would get an empty list from dropWhile which would cause head to 
-- throw an error. Instead use `find` and return a `Maybe`.
--  
--  dateStockExceeded stocks n = (year, month, day)
--    where (_, year, month, day) = head $ dropWhile (\(val, _, _, _) -> val > n) stocks
dateStockExceeded n = find (\(val, _, _, _) -> n < val) 

splitByFirstSpace :: String -> (String, String)
splitByFirstSpace = span (/= ' ')

splitByFirstSpace' :: String -> (String, String)
splitByFirstSpace' = break' (== ' ')

break' :: Eq a => (a -> Bool) -> [a]-> ([a], [a])
break' p = span (not . p)

-- sort, list elms must conform to Ord
-- group

countUniqueElms :: Ord a => [a] -> [(Int, a)]
countUniqueElms = map (\l@(x:xs) -> (length l, x)) . group. sort

-- inits and tails - recurrsively applies init and tail 

isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf' needle haystack =
  let nLen = length needle 
  in foldl (\acc cand -> if (take nLen cand) == needle then True else acc) False (tails haystack) 

-- isInfixOf, isPrefixOf, isSuffixOf
-- elem and notElem
-- partition
-- find
-- elemIndex amd elemIndices
-- zip[3..7] and zipWith[3..7]
-- lines and unlines
-- words and unwords
-- delete 
-- \\ - set difference!
-- union - note that duplicates are removed from the second list
-- intersect
-- insert
-- genericTake, genericDrop, etc
