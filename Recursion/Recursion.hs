maximum1 :: Ord a => [a] -> a
maximum1 [] = error "Empty set"
maximum1 [a] = a
maximum1 (head:xs)
  | head > maxTail  = head
  | otherwise       = maxTail
  where maxTail     = maximum xs

maximum2 :: Ord a => [a] -> a
maximum2 []   = error "Empty list"
maximum2 [x]  = x
maximum2 (head:tail) = max head (maximum2 tail)

-- In essence, the maximum of a list is the great of the
-- head and the maximum of the tail

-- Because Num isn't a subclass of Ord, we need to specify
-- both class constraints in the type definition (we're both
-- subtracting and comparing)
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n a
  | n <= 0    = []
  | otherwise = a:replicate' (n - 1) a

-- Note that the first guard without the otherwise falls
-- through to the next pattern if it doesn't match.
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0      = []
take' _ []     = []
take' n (x:xs)  = x:take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (a:xs) = reverse' xs ++ [a]

repeat' :: a -> [a]
repeat' a = a:repeat' a

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (aHead:aTail) (bHead:bTail) = (aHead,bHead):zip' aTail bTail

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = x == a || elem' a xs

quicksort' :: Ord a => [a] -> [a]
quicksort' []      = []
quicksort' (x:xs)  =
  let lowerElms   = quicksort' [a | a <- xs, a <= x]
      higherElms  = quicksort' [a | a <- xs, a > x]
  in  lowerElms ++ [x] ++ higherElms
