import Data.List

data Section = Section {getA :: Int,
                        getB :: Int,
                        getC :: Int } deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

createSection :: [Int] -> Section
createSection [a,b,c] = Section a b c
createSection _ = Section 0 0 0

totalPathCost :: Path -> Int
totalPathCost = sum . (map snd)

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section aCost bCost cCost) =
  let totalCostA = totalPathCost pathA
      totalCostB = totalPathCost pathB
      forwardToACost = totalCostA + aCost
      forwardToBCost = totalCostB + bCost
      crossToACost = totalCostB + bCost + cCost
      crossToBCost = totalCostA + aCost + cCost
      bestAPath = if forwardToACost <= crossToACost
                     then (A, aCost):pathA
                     else (C, cCost):(B, bCost):pathB
      bestBPath = if forwardToBCost <= crossToBCost
                     then (B, bCost):pathB
                     else (C, cCost):(A, aCost):pathA
  in (bestAPath, bestBPath)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
   in if (totalPathCost bestAPath) <= (totalPathCost bestBPath)
         then reverse bestAPath
         else reverse bestBPath

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
  contents <- getContents
  let sectionNumbers = groupsOf 3 (map read $ lines contents)
  let roadSystem = map createSection sectionNumbers
      bestPath = optimalPath roadSystem
      pathStr = concat $ map (show . fst) bestPath
      pathPrice = totalPathCost bestPath
  putStrLn $ "Best path: " ++ pathStr
  putStrLn $ "Price of path: " ++ show pathPrice
