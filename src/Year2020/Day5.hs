module Year2020.Day5
( getAnswerPart1 
, getAnswerPart2
) where

import Debug.Trace (trace)

getAnswerPart1 :: String -> String
getAnswerPart1 input = 
    let seatIds = [ trace ("Result of getColumn: " ++ show columnResult)
                  $ trace ("Result of getRow: " ++ show rowResult)
                  $ columnResult + 8 * rowResult
                  | line <- lines input
                  , let columnResult = getColumn (take 7 line) 0 127
                  , let rowResult = getRow (drop (length line - 3) line) 0 7
                  ]
    in show $ maximum seatIds

getAnswerPart2 :: String -> String
getAnswerPart2 input = ""

getColumn :: String -> Int -> Int -> Int
getColumn [] start _ = start
getColumn (x:xs) start end
    | x == 'F' && end - start == 1 = start
    | x == 'B' && end - start == 1 = end
    | x == 'F' = getColumn xs start ((start + end) `div` 2)
    | x == 'B' = getColumn xs ((start + end) `div` 2) end
    | otherwise = error "Invalid character in input string"

getRow :: String -> Int -> Int -> Int
getRow [] start _ = start
getRow (x:xs) start end
    | x == 'L' && end - start == 1 = start
    | x == 'R' && end - start == 1 = end
    | x == 'L' = getRow xs start ((start + end) `div` 2)
    | x == 'R' = getRow xs ((start + end) `div` 2) end
    | otherwise = error "Invalid character in input string"
