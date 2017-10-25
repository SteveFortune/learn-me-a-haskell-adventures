import Data.List
import Text.Read

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
