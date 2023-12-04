module Year2023.Day2 (
    getAnswerPart1,
    getAnswerPart2,
) where

import Data.List.Split (splitOn)
import Data.Map (Map, elems, empty, insert, lookup)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)

type Round = Map String Int

getAnswerPart1 :: String -> String
getAnswerPart1 input =
    let lines' = lines input
        gameIds = map gameIdIfPossible lines'
     in show $ sum $ catMaybes gameIds

getAnswerPart2 :: String -> String
getAnswerPart2 input =
    let lines' = lines input
        products = map minimumCubesProduct lines'
     in show $ sum $ products

gameIdIfPossible :: String -> Maybe Int
gameIdIfPossible line =
    let firstSplit = splitOn ": " line
        gameId = read $ last $ splitOn " " (head firstSplit) :: Int
        roundStrings = splitOn "; " (last firstSplit)
        rounds = map parseRound roundStrings
        result = all roundPredicate rounds
     in if result then Just gameId else Nothing

minimumCubesProduct :: String -> Int
minimumCubesProduct line =
    let firstSplit = splitOn ": " line
        roundStrings = splitOn "; " (last firstSplit)
        rounds = map parseRound roundStrings
        keys' = Data.Map.elems $ powerCubes rounds
     in product keys'

roundPredicate :: Round -> Bool
roundPredicate round' =
    all
        ( \(color, count) ->
            let cube = Data.Map.lookup color round'
             in case cube of
                    Just existingCount -> trace (show $ existingCount <= count) existingCount <= count
                    Nothing -> trace (show True) True
        )
        bagContents

updateAccRound :: String -> Round -> Round -> Round
updateAccRound color round' accRound' =
    let value = Data.Map.lookup color round'
        accValue = Data.Map.lookup color accRound'
     in case value of
            Just count -> case accValue of
                Just accCount -> if count > accCount then insert color count accRound' else accRound'
                Nothing -> insert color count accRound'
            Nothing -> accRound'

powerCubes :: [Round] -> Round
powerCubes =
    foldr
        ( \round' accRound ->
            foldr (`updateAccRound` round') accRound ["red", "green", "blue"]
        )
        empty

parseRound :: String -> Round
parseRound gameString =
    let cubeStrings = splitOn ", " gameString
     in foldr
            ( \pairString cubeMap ->
                let pair = splitOn " " pairString
                    key = last pair
                    value = read $ head pair :: Int
                 in insert key value cubeMap
            )
            empty
            cubeStrings

bagContents :: [(String, Int)]
bagContents = [("red", 12), ("green", 13), ("blue", 14)]
