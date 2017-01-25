
-- Double me functions

doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleUsWithMe x y = doubleMe x + doubleMe y

-- Double me if
-- If stmts are expressions in Haskell - they require an
-- else and can be written on one line. E.g.
--  
--    doubleSmallNumber x = if x > 100 then x else doubleMe x
doubleSmallNumber x = if x > 100
                        then x
                        else doubleMe x

-- Apostrophes are valid characters in Haskell functions
doubleYa'Number x = (if x > 100 then x else doubleMe x) + 1
-- Functions that don't take parameters are definitions - i.e
-- we can't change what they return.
conanO'Brien = "I'm so irish"

-- Note that when concatinating lists with ++, Haskell has to 
-- walk through the whole list of the lefthand side, so not 
-- very performant
combineUs a b = a ++ b
pushMe a list = a:list
elmAt i list = list !! i

-- Other useful list operations:
-- - Comparators: <, <=, >, >=, ==, /= (lexicographical order)
-- - head [1,2,3]
-- - tail [3,2,1]
-- - last [1,2,3]
-- - init [1,2,3]
--
-- Init,                  Last
-- |---------------------|--|
-- [1,2,3,4,5,6,7,8,9,10,11]
-- |-|----------------------|
-- Head, tail
--
-- - Check length: length [1,2,3]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]
nouns = ["apples", "oranges", "bikers", "hippies"]
adjectives = ["stupid", "infantile", "duplicitus", "damn"]
crazyPhrases = [adj ++ " "  ++ noun | adj <- adjectives, noun <- nouns ]
filteredPhrases letter = [ phrase | phrase <- crazyPhrases, letter `elem` phrase ]
length' list = sum [ 1 | _ <- list ]
upper = ['A'..'Z']
lower = ['a'..'z']

filterLower str = [letter | letter <- str, not (letter `elem` lower)]

-- Really cool example of starting with the set of all possible 
-- instances and then applying conditions in the set builder.
isRightAngle a b c = a^2 + b^2 == c^2
findRightAngle maxSideLen perimeter = [(a,b,c) | c <- [1..maxSideLen], b <- [1..c], a <- [1..b], (isRightAngle a b c), a + b + c == perimeter]
