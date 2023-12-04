module Year2023.Day3 (
    getAnswerPart1,
    getAnswerPart2,
    extractDigits,
) where

import Data.Char (isDigit)
import Data.List (groupBy, sortBy)

getAnswerPart1 :: String -> String
getAnswerPart1 input =
    let width = length $ head $ lines input
        str = filter (/= '\n') input
        numbers = extractDigits str (length str)
        filtered = filter (isPartNumber str width) numbers
        partNumbers = map fst filtered
     in show $ sum $ partNumbers

getAnswerPart2 :: String -> String
getAnswerPart2 input =
    let width = length $ head $ lines input
        str = filter (/= '\n') input
        numbers = extractDigits str (length str)
        possibleGears = concatMap (adjacentGears str width) numbers
        sortedGears = sortBy (\(i, _) (j, _) -> compare i j) possibleGears
        gears = filter ((== 2) . length) (groupBy (\(i, _) (j, _) -> i == j) sortedGears)
        gearProducts = map (foldr (\(_, a) acc -> acc * a) 1) gears
     in show $ sum gearProducts

isPartNumber :: String -> Int -> (Int, Int) -> Bool
isPartNumber str width (number, i) =
    let numberLength = length $ show number
        spacesToCheck = [-1, numberLength] ++ [-(width + 1) .. (-width + numberLength)] ++ [(width - 1) .. (width + numberLength)]
     in any
            ( \j ->
                let spaceIndex = i + j
                    isInList = spaceIndex >= 0 && spaceIndex < length str
                 in (isInList && ((str !! spaceIndex) `notElem` ".0123456789"))
            )
            spacesToCheck

adjacentGears :: String -> Int -> (Int, Int) -> [(Int, Int)]
adjacentGears str width (number, i) =
    let numberLength = length $ show number
        spacesToCheck = [-1, numberLength] ++ [-(width + 1) .. (-width + numberLength)] ++ [(width - 1) .. (width + numberLength)]
     in foldr
            ( \j gears ->
                let spaceIndex = i + j
                    isInList = spaceIndex >= 0 && spaceIndex < length str
                 in if isInList && ((str !! spaceIndex) `elem` "*")
                        then (spaceIndex, number) : gears
                        else gears
            )
            []
            spacesToCheck

extractDigits :: String -> Int -> [(Int, Int)]
extractDigits [] _ = []
extractDigits str len =
    let digitStart = dropWhile (not . isDigit) str
        (digitList, rest) = span isDigit digitStart
     in if length digitList == 0
            then []
            else
                let number = read digitList
                    i = len - (length digitList + length rest)
                 in ((number, i) : extractDigits rest len)
