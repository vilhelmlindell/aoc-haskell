module Year2020.Day1
( getAnswerPart1 
, getAnswerPart2
) where

getAnswerPart1 :: String -> String
getAnswerPart1 input =
    let numbers = map read $ lines input :: [Int]
        pairs = [(x, y) | x <- numbers, y <- numbers, x + y == 2020]
    in case pairs of
        [] -> "No pair found"
        (x, y):_ -> show $ x * y

getAnswerPart2 :: String -> String
getAnswerPart2 input =
    let numbers = map read $ lines input :: [Int]
        pairs = [(x, y, z) | x <- numbers, y <- numbers, z <- numbers, x + y + z == 2020]
    in case pairs of
        [] -> "No pair found"
        (x, y, z):_ -> show $ x * y * z
