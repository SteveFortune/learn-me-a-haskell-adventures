
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
