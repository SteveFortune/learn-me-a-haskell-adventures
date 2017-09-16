multThree :: Num a => a -> a -> a -> a
multThree a b c = a * b * c

-- Just (Eq a) doesn't work as (==) 2 will return a
-- function of type (Num a, Eq a) => a -> Bool, which
-- must match the type of eq2
eq2 :: (Num a, Eq a) => a -> Bool
eq2 = (==) 2

divideByTen :: Floating a => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- -> is right associative, so it requires parenthesis 
-- sometimes

applyTwice :: (a -> a) -> a -> a
applyTwice fn x = fn (fn x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' zipf (x:xs) (y:ys) = zipf x y : zipWith' zipf xs ys

-- flip' f = g where g x y = f y x

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' fn x y = fn y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' fn (x:xs) = fn x : map' fn xs 

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

chain :: Integral a => a -> [a]
chain 1 = [1]
chain n 
  | even n  = n:chain (n `div` 2)
  | odd n   = n:chain (n*3 + 1)

chainsWithLen :: Integral a => a -> Int -> Int
chainsWithLen n lower
  | n <= 1    = error "Cannot be <= 1"
  | otherwise = length [x | x <- [1..n], isLongEnough (chain x)]
  where isLongEnough xs = length xs > lower

chainsWithLen2 :: Integral a => a -> Int -> Int
chainsWithLen2 n lower 
  | n <= 1    = error "Cannot be <= 1"
  | otherwise = length (filter isLongEnough (map chain [1..n])) 
  where isLongEnough xs = length xs > lower

-- A partially applied map function that maps an array of 
-- numbers to functions that * an input by the numbers.
-- Partially applied map function that's passed a partially
-- applied * function.

mapList :: Num a => [a] -> [a -> a]
mapList = map (*)

-- Lambdas! 

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- You can pattern match in lambdas 

mapAdd :: Num a => [(a, a)] -> [a]
mapAdd = map (\(x,y) -> x + y)

-- Using foldl to reduce a value from a list

sum' :: Num a => [a] -> a
sum' = foldl (+) 0 -- (\acc x -> acc + x) 0 xs

elem' :: Eq a => a -> [a] -> Bool
elem' y = foldl (\acc x -> acc || x == y) False

-- ++ is expensive, so best to use right folds when building 
-- up new lists from a list.
-- Right holds also work on infinite lists whereas left folds 
-- dont.

map2' :: (a -> b) -> [a] -> [b]
map2' f = foldr ((:) . f) []

sum2' :: Num a => [a] -> a
sum2' = foldl1 (+)

reverse2' :: [a] -> [a]
reverse2' = foldl (flip (:)) []

-- How many elements does it take for the sum of the roots of 
-- all natural numbers to exceed n?

sqrtSums :: Double -> Int
sqrtSums n = length (takeWhile (<= n) (scanl1 (+) (map sqrt [1..]))) + 1

fn = ceiling . negate . tan . cos . max 50 -- x = ceiling (negate (tan (cos (max 50 x))))
