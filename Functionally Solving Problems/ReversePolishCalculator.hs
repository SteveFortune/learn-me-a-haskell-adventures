import Data.List
import Text.Read

-- This is my initial attempt. After reading through the rest of
-- the section in the chapter, it's clear that there's a much more
-- eloquent / flexible way of doing it.
-- This seems to be a bit of a pattern - all of the solutions I've
-- come up with myself seem to be that bit more complicated than
-- they need to by. Why?
-- - Still getting use to a lot of the language features, many of
--   which I have never seen before. Practice makes perfect etc

type OperatorToken = String
type Operator = Double -> Double -> Double

operatorTokens :: [(OperatorToken, Operator)]
operatorTokens =
  [("*", (*)),
   ("+", (+)),
   ("-", (-)),
   ("/", (/))
  ]

solveRPN :: String -> Double
solveRPN expr =
  head (foldl parseToken [] tokens)
  where tokens = words expr

parseToken :: [Double] -> OperatorToken -> [Double]
parseToken stack token =
  case (readMaybe token :: Maybe Double) of
    Just value  -> value:stack
    Nothing     ->
      case lookup token operatorTokens of
        Just op   -> if length stack > 1
                        then let rh:lh:xs = stack
                              in (lh `op` rh):xs
                        else stack
        Nothing   -> stack

-- This is the book-provided solution. Much cleaner.
-- Why?
-- - Can define operators of any arity, not restricted to
--   binary.
-- - Less code - does the same thing in a more concise way.

solveRPN' :: String -> Double
solveRPN' = head . foldl parseToken [] . words
  where parseToken stack "*" = applyBinaryOp stack (*)
        parseToken stack "/" = applyBinaryOp stack (/)
        parseToken stack "+" = applyBinaryOp stack (+)
        parseToken stack "-" = applyBinaryOp stack (-)
        parseToken stack "^" = applyBinaryOp stack (**)
        parseToken stack "ln" = applyUnaryOp stack log
        parseToken stack "sum" = [sum stack]
        parseToken stack numberString =
          case (readMaybe numberString :: Maybe Double) of
            Just number   -> number:stack
            Nothing       -> stack
        applyBinaryOp (rh:lh:xs) op = (lh `op` rh):xs
        applyBinaryOp xs op = xs
        applyUnaryOp (o:xs) op = (op o):xs
        applyUnaryOp xs op = xs

