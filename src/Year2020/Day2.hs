module Year2020.Day2
( getAnswerPart1 
, getAnswerPart2
) where

import Data.List (find)
import Data.List.Split (splitWhen)
import Data.Maybe (fromJust)
import Data.Char (isDigit, isLetter)

getAnswerPart1 :: String -> String
getAnswerPart1 input = 
    let validCount = length $ filter matchesPolicy1 $ lines input
    in show validCount

getAnswerPart2 :: String -> String
getAnswerPart2 input = 
    let validCount = length $ filter matchesPolicy2 $ lines input
    in show validCount

matchesPolicy1 :: String -> Bool
matchesPolicy1 str =
    let letter = fromJust $ find isLetter str
        minOccurences = head $ extractNumbers str
        maxOccurences = extractNumbers str !! 1
        occurences = length . filter (== letter) $ last $ words str
    in (occurences >= minOccurences) && (occurences <= maxOccurences)

matchesPolicy2 :: String -> Bool
matchesPolicy2 str =
    let letter = fromJust $ find isLetter str
        position1 = head (extractNumbers str) - 1
        position2 = (extractNumbers str !! 1) - 1
        password = last $ words str
    in (password !! position1 == letter && password !! position2 /= letter) || 
       (password !! position1 /= letter && password !! position2 == letter)

extractNumbers :: String -> [Int]
extractNumbers str = [read num | num <- splitWhen (not . isDigit) str, all isDigit num]
