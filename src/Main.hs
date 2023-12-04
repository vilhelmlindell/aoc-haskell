module Main (
    main,
) where

import Year2020.Day
import Year2023.Day

type Year = (Int, Int) -> String -> String

getYear :: Int -> Year
getYear year = case year of
    2020 -> Year2020.Day.getAnswerForDay
    2023 -> Year2023.Day.getAnswerForDay
    _ -> error "No such year"

yearInputPath :: Int -> (Int -> FilePath)
yearInputPath year = case year of
    2020 -> Year2020.Day.getInputFilePath
    2023 -> Year2023.Day.getInputFilePath
    _ -> error "No such year"

main :: IO ()
main = do
    putStrLn "Enter the year number (2015-2023)"
    yearNumberStr <- getLine
    let yearNumber = read yearNumberStr :: Int

    putStrLn "Enter the day number (1-24):"
    dayNumberStr <- getLine
    let dayNumber = read dayNumberStr :: Int

    putStrLn "Enter the part number (1-2):"
    partNumberStr <- getLine
    let partNumber = read partNumberStr :: Int

    let inputPath = yearInputPath yearNumber dayNumber

    contents <- readFile inputPath
    let answer = getYear yearNumber (dayNumber, partNumber) contents
    putStrLn answer
