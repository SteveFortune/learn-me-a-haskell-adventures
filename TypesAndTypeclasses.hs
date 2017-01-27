
isRightAngle :: Int -> Int -> Int -> Bool
isRightAngle a b c = a^2 + b^2 == c^2

findRightAngle :: Int -> Int -> [(Int, Int, Int)]
findRightAngle maxSideLen perimeter = [(a,b,c) | c <- [1..maxSideLen], b <- [1..c], a <- [1..b], (isRightAngle a b c), a + b + c == perimeter]

addSomeNumbers :: Int -> Int -> Int -> Int
addSomeNumbers a b c = a + b + c

factorial :: Integer -> Integer 
factorial n = product [1..n]

circumference :: Float -> Float 
circumference r = 2 * pi * r

circumference' :: Double -> Double 
circumference' r = 2 * pi * r

-- Float - floating point
-- Double - floating point with double precision
-- Int - +/- 2 ^ 31
-- Integer - big integer
-- Bool - boolean
-- Char - character
-- String - [Char]

-- a and b are type variables
-- Functions that have type variables are polymorphic functions
fst' :: (a,b) -> a
fst' t = fst t

-- Pretty much all operators are functions (==, /=, etc.) 
-- Functions comprised of special characters are considered infix
-- functions by default.

eq :: (Eq a) => a -> a -> Bool
eq e1 e2 = e1 == e2

gt :: (Ord a) => a -> a -> Bool
gt e1 e2 = e1 > e2

-- Other typeclasses:
-- - Show: can be represented as a string
-- - Read: a type which can be marshalled from a string, e.g. read "True" || False
-- -- read "4" throws an error because the compiler can't infer which type to convert 
--    the string to
-- -- read "4" :: Int explicitly annotates which type an expression should be
-- - Enum: sequentially ordered types, can be enumerated and used in list ranges
-- -- Also have defined successors and predecesors
-- - Bounded: have upper and lower bounds
-- -- minBound and maxBound
-- -- Type of Bounded a => a
-- -- Polymorphic constants 
-- -- Tuples are also in it
-- - Num: members act like numbers. All arithmetic operators act on Nums 
-- -- Type of a literal is a polymorphic constant-



