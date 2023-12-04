module Year2023.Day4 (
    getAnswerPart1,
    getAnswerPart2,
) where

import Data.List (intersect)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, fromList, insert, keys, lookup, singleton)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

getAnswerPart1 :: String -> String
getAnswerPart1 input =
    let lines' = lines input
        numbers = map scratchNumbers lines'
        winCount =
            map
                ( \cards ->
                    let wins = cardWins cards
                     in if wins == 0 then 0 else 2 ^ (wins - 1)
                )
                numbers
     in show $ sum winCount

getAnswerPart2 :: String -> String
getAnswerPart2 input =
    let lines' = lines input
        numbers = map scratchNumbers lines'
        winCount = map cardWins numbers
        scratchCards = scratchCardCount (zip [1 ..] winCount)
        scratchCardCounts = map (fromMaybe 0 . (`Data.Map.lookup` scratchCards)) (keys scratchCards)
     in show $ sum (scratchCardCounts)

scratchCardCount :: [(Int, Int)] -> Map Int Int
scratchCardCount indexedCardWins =
    foldl
        ( \cards (i, matches) ->
            let cardCopies = [(i + 1) .. (i + matches)]
                cardCount = fromMaybe 0 (Data.Map.lookup i cards)
                newCards =
                    if matches == 0
                        then insert i cardCount cards
                        else addCardCopies cardCopies cardCount cards
             in trace (show newCards) newCards
        )
        ( fromList
            (zip [1 .. (length indexedCardWins)] (repeat 1))
        )
        indexedCardWins

addCardCopies :: [Int] -> Int -> Map Int Int -> Map Int Int
addCardCopies cardCopies cardCount cardMap =
    foldr
        ( \cardCopy cards ->
            let value = fromMaybe 0 (Data.Map.lookup cardCopy cards)
             in insert cardCopy (value + cardCount) cards
        )
        cardMap
        cardCopies

scratchNumbers ::
    String ->
    ([Int], [Int])
scratchNumbers str =
    let splitStr = splitOn " | " str
        myNumbersStr = words (last splitStr)
        myNumbers = map (read :: String -> Int) myNumbersStr
        winningNumbersStr = (last $ splitOn ": " (head splitStr))
        winningNumbers = map (read :: String -> Int) (words winningNumbersStr)
     in (winningNumbers, myNumbers)

cardWins :: ([Int], [Int]) -> Int
cardWins (winningNumbers, myNumbers) = (length $ intersect myNumbers winningNumbers)
