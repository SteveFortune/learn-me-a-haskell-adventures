import Data.List
-- import Data.List hiding (nub)
import qualified Data.Map as M
import qualified Data.Set as S
-- M.filter, M.null, etc
import Data.Function (on)
import Data.Char

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

-- Initially, written as:
--  groupByPve = groupBy (\x y = (x > 0) == (y > 0))
-- The `on` function applies its first fn parameter to the result
-- of its second fn parameter invoked with x and y:
--  f `on` g = (\x y) -> f (g x) (g y)
-- Which is an abstraction of `(x > 0) == (y > 0)`
groupByPve :: (Ord a, Num a) => [a] -> [[a]]
groupByPve = groupBy ((==) `on` (> 0))

sortByLen :: [[a]] -> [[a]]
sortByLen = sortBy (compare `on` length)

words' :: String -> [String]
words' = filter (not . any isSpace) . groupBy ((==) `on` isSpace)

upperCaseMe :: String -> String
--upperCaseMe = unwords . map (\(x:xs) -> (toUpper x):xs) . words
upperCaseMe = foldr (\c str -> if null str || not (isSpace c)
                                  then c:str
                                  else let head:rest = str in c:(toUpper head):rest) ""

-- I am a composition cowboy
encode :: Int -> String -> String
encode shift = map (chr . (+ shift) . ord)

decode :: Int -> String -> String
decode shift = encode (negate shift)

lookup' :: Eq k => k -> [(k,v)] -> Maybe (k,v) -- Maybe v
-- lookup' _ [] = Nothing
-- lookup' key ((k,v):xs) = if key == k then Just v else lookup' key xs
-- lookup' key = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing
lookup' key = find ((== key) . fst)

fromList' :: Ord k => [(k,v)] -> M.Map k v
fromList' = foldr (\(k,v) acc -> M.insert k v acc) M.empty

keys' :: Ord k => M.Map k v -> [k]
keys' = map fst . M.toList

elems' :: Ord k => M.Map k v -> [v]
elems' = map snd . M.toList

numbers = [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

fromListWithBiggest :: (Ord a) => [(a, a)] -> M.Map a a
fromListWithBiggest = M.fromListWith max

fromListWithPlus :: (Ord k, Num v) => [(k, v)] -> M.Map k v
fromListWithPlus = M.fromListWith (+)

-- Set.isSubsetOf, Set.isProperSubsetOf
