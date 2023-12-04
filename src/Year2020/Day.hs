-- In Day.hs
module Year2020.Day
    ( getAnswerForDay
    , getInputFilePath
    ) where

import Year2020.Day1 as Day1
import Year2020.Day2 as Day2
import Year2020.Day3 as Day3
import Year2020.Day4 as Day4
import Year2020.Day5 as Day5
import Year2020.Day6 as Day6

getAnswerForDay :: (Int, Int) -> String -> String
getAnswerForDay (dayNumber, partNumber) input = case (dayNumber, partNumber) of
    (1, 1) -> Day1.getAnswerPart1 input
    (1, 2) -> Day1.getAnswerPart2 input
    (2, 1) -> Day2.getAnswerPart1 input
    (2, 2) -> Day2.getAnswerPart2 input
    (3, 1) -> Day3.getAnswerPart1 input
    (3, 2) -> Day3.getAnswerPart2 input
    (4, 1) -> Day4.getAnswerPart1 input
    (4, 2) -> Day4.getAnswerPart2 input
    (5, 1) -> Day5.getAnswerPart1 input
    (5, 2) -> Day5.getAnswerPart2 input
    (6, 1) -> Day6.getAnswerPart1 input
    (6, 2) -> Day6.getAnswerPart2 input
    _ -> "Day not implemented yet"

-- Function to generate the input file path for a given day number
getInputFilePath :: Int -> FilePath
getInputFilePath dayNumber = "input/Year2020/Day" ++ show dayNumber ++ ".txt"
