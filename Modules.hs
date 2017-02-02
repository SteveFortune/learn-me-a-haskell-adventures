import Data.List (nub, sort, intersperse, intercalate)
-- import Data.List hiding (nub)
import qualified Data.Map as M
-- M.filter, M.null, etc

numUniques :: Eq a => [a] -> Int
numUniques = length . nub

-- intersperse 

dotMe :: String -> String
dotMe = intersperse '.'

-- intercalate 

spaceMe :: [String] -> String 
spaceMe = intercalate " "

-- transpose 


