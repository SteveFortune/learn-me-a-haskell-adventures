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
