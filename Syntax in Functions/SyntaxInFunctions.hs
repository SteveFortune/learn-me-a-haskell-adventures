
-- Pattern matching

lucky :: Integral a => a -> String
lucky 7 = "Lucky number 7!"
lucky x = "Not so lucky"

-- Note that they are in order of matching. If we moved
-- sayMe x to the top, it would catch everything.

sayMe :: Integral a => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Not between 1 and 3."

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

brokenFn :: Char -> String
brokenFn 'a' = "Amanda"
brokenFn 'b' = "Banana"

-- Pattern matching with tuples!

badAddVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
badAddVectors a b = (fst a + fst b, snd a + snd b)

addVectors :: Num a => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- Pattern matching also works with list comprehension

addTuples :: (Num a) => [(a, a)] -> [a]
addTuples xs = [a + b | (a, b) <- xs]

-- You can match with the empty list and patterns that
-- involve :

head3 :: [a] -> (a, a, a)
head3 [] = error "Empty array"
head3 (a1:a2:a3:_) = (a1, a2, a3)

tell :: (Show a) => [a] -> String
tell [] = error "Empty list, nothing to show"
tell (x:[]) = "First and only element is " ++ show x
tell (x:y:[]) = "Second: " ++ show y ++ " and first: " ++ show x
tell (x:y:_) = "The list is long, but first: " ++ show x ++ " and second: " ++ show y

-- NB: tell (x:[]) could be written as tell [x]

length' :: Num b => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: Num b => [b] -> b
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 'as pattern' can be used to reference the variable that
-- you decompose in a pattern

capital :: String -> String
capital str@(x:_) = "The first char of " ++ str ++ " is: " ++ [x]

-- Guards
-- - Nice alternatives for big if/else trees
-- - Expressions that evaulate to bool; unlike pattern matching
--   which only checks whether a variable satisfied some pattern.
--   E.g. could do some arithmetic.
-- If no guard evaluates to true, evaluation falls through to the
-- next *pattern*.
-- You can use guards inline, but it'a not very readable.

bmiTell :: RealFloat a => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're thin"
  | bmi <= 25.0 = "You're freakishly normal"
  | bmi <= 30.0 = "Fat"
  | otherwise = "Takes up 2 seats on a train"

bmiTell2 :: RealFloat a => a -> a -> String
bmiTell2 weight height
  | bmi <= thin = "You're thin"
  | bmi <= normal = "You're freakishly normal"
  | bmi <= fat = "Fat"
  | otherwise = "Takes up 2 seats on a train"
  where bmi = weight / height ^ 2
        (thin, normal, fat) = (18.5, 25.0, 30.0)

max' :: Ord a => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b

compare' :: Ord a => a -> a -> Ordering
a `compare'` b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT

dot :: Char -> String
dot ch = [ch] ++ ". "

-- Cleaner to define as intials (f:_) (h:_) ... but demonstrates
-- pattern matching in where

initials :: String -> String -> String
initials firstname lastname = dot f ++ dot l
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: RealFloat b => [(b, b)] -> [b]
calcBmis xs =
  [bmi w h | (w, h) <- xs]
  where bmi w h = w / h ^ factor where factor = 2

-- Let bindings are similar to where bindings, but let you
-- bind variables anywnere are restrict them to their scope.
-- They are also expressions, so `let <bindings> in <expression>`
-- evaluates to <expression>
-- Can be used to introduction functions in local scope:
--  [let sq x = x * x in (sq 2, sq 4, sq 10)]

cylinderSA :: RealFloat a => a -> a -> a
cylinderSA r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + topArea * 2

calcBmi2 :: RealFloat a => [(a, a)] -> [a]
calcBmi2 bmis = [bmi | (w, h) <- bmis, let bmi = w / h ^ 2]

fatPeopleBmis :: RealFloat a => [(a, a)] -> [a]
fatPeopleBmis bmis = [bmi | (w,h) <- bmis, let bmi = w / h ^ 2, bmi >= 25.0]

-- case <expression> of <pattern> -> <result>
--                      <pattern> -> <result>

describeList :: [a] -> String
describeList xs = "This is a list of " ++ case xs of [] -> "nothing."
                                                     [_] -> "a single item."
                                                     xs -> "lots of items."



