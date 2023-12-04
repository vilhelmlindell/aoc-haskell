module Year2023.Day (
    getAnswerForDay,
    getInputFilePath,
) where

import Year2023.Day1 as Day1
import Year2023.Day2 as Day2
import Year2023.Day3 as Day3
import Year2023.Day4 as Day4

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
    _ -> "Day not implemented yet"

getInputFilePath :: Int -> FilePath
getInputFilePath dayNumber = "input/Year2023/Day" ++ show dayNumber ++ ".txt"
