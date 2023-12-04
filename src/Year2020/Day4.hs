module Year2020.Day4
( getAnswerPart1 
, getAnswerPart2
) where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf, tails, findIndex)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Debug.Trace (trace)

countValidPassports :: (String -> Bool) -> String -> Int
countValidPassports isValidPassport input =
    length [1 :: Int | passport <- splitOn "\n\n" input, isValidPassport passport]

getAnswerPart1 :: String -> String
getAnswerPart1 input =
    show $ countValidPassports isPassportValid1 input

getAnswerPart2 :: String -> String
getAnswerPart2 input =
    show $ countValidPassports isPassportValid2 input

isPassportValid1 :: String -> Bool
isPassportValid1 passport = 
    let validFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    in all (`isInfixOf` passport) validFields

isPassportValid2 :: String -> Bool
isPassportValid2 passport =
    let validFields = [("byr", isByrValid), ("iyr", isIyrValid), ("eyr", isEyrValid), ("hgt", isHgtValid), ("hcl", isHclValid), ("ecl", isEclValid), ("pid", isPidValid)]
    in all (\(fieldName, validationFunc) ->
        case findString fieldName passport of
            Just index -> do
                let fieldValue = takeWhile (`notElem` " \n") $ drop (index + 4) passport
                let result = validationFunc fieldValue
                trace ("Field Name: " ++ fieldName ++ ", Field Value: " ++ fieldValue) $
                    trace ("Validation Result: " ++ show result) result
            Nothing    -> False
    ) validFields

isByrValid :: String -> Bool
isByrValid str = 
    case readMaybe str  :: Maybe Int of
        Just year -> year >= 1920 && year <= 2002
        Nothing   -> False

isIyrValid :: String -> Bool
isIyrValid str =
    case readMaybe str  :: Maybe Int of
        Just year -> year >= 2010 && year <= 2020
        Nothing   -> False

isEyrValid :: String -> Bool
isEyrValid str =
    case readMaybe str  :: Maybe Int of
        Just year -> year >= 2020 && year <= 2030
        Nothing   -> False

isHgtValid :: String -> Bool
isHgtValid str =
    case readMaybe (dropLast 2 str) :: Maybe Int of
        Just height ->
            (("cm" `isSuffixOf` str) && (height >= 150) && (height <= 193)) ||
            (("in" `isSuffixOf` str) && (height >= 59) && (height <= 76))
        Nothing -> False

isHclValid :: String -> Bool
isHclValid str =
    (head str == '#' && length str == 7) &&
    all (`elem` "0123456789abcdef") (drop 1 str)

isEclValid :: String -> Bool
isEclValid str = 
    let options = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    in elem str options

isPidValid :: String -> Bool
isPidValid str =
    case readMaybe str :: Maybe Int of
        Just _ -> length str == 9
        Nothing -> False

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

dropLast :: Int -> [a] -> [a]
dropLast n xs
    | n >= 0 = take (length xs - n) xs
    | otherwise = xs
