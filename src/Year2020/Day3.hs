module Year2020.Day3
( getAnswerPart1 
, getAnswerPart2
) where

getAnswerPart1 :: String -> String
getAnswerPart1 input = show $ getTreeCount (lines input) 0 3 1

getAnswerPart2 :: String -> String
getAnswerPart2 input = 
    let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        treeCounts = map (uncurry (getTreeCount (lines input) 0)) slopes
    in show $ product treeCounts

getTreeCount :: [String]-> Int -> Int -> Int -> Int
getTreeCount [] _ _ _ = 0
getTreeCount treeMap column right down = 
    let row = head treeMap
        letter = row !! (column `mod` length row)
    in getTreeCount (drop down treeMap) (column + right) right down + if letter == '#' then 1 else 0
