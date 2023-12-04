module Year2023.Day1 (
    getAnswerPart1,
    getAnswerPart2,
    matchesPredicate,
    allPredicateMatches,
) where

import Text.Read (readMaybe)

getAnswerPart1 :: String -> String
getAnswerPart1 input =
    let lines' = lines input
        matching = map (\line -> allPredicateMatches line (readMaybe :: String -> Maybe Int) 0) lines'
        numbers = map (\lst -> read (show (fst $ head lst) ++ show (fst $ last lst)) :: Int) matching
     in show $ sum numbers

getAnswerPart2 :: String -> String
getAnswerPart2 input =
    let lines' = lines input
        matching1 = map (\line -> allPredicateMatches line (readMaybe :: String -> Maybe Int) 0) lines'
        matching2 = map (\line -> allPredicateMatches line digitString 0) lines'
        pairs1 = map (\pairs' -> (head pairs', last pairs')) matching1
        pairs2 =
            map
                ( \pairs' ->
                    if length pairs' == 0
                        then (Nothing, Nothing)
                        else (Just $ head pairs', Just $ last pairs')
                )
                matching2

        numbers =
            zipWith
                ( \pair1 pair2 ->
                    ( let
                        ((d1, i1), (d2, i2)) = pair1
                        (a, b) = pair2
                        f = case a of
                            Just (d, i) -> if i < i1 then d else d1
                            Nothing -> d1
                        l = case b of
                            Just (d, i) -> if i > i2 then d else d2
                            Nothing -> d2
                       in
                        read (show f ++ show l) :: Int
                    )
                )
                pairs1
                pairs2
     in show $ sum numbers

allPredicateMatches :: String -> (String -> Maybe a) -> Int -> [(a, Int)]
allPredicateMatches [] _ _ = []
allPredicateMatches line@(_ : xs) predicate n =
    let result = matchesPredicate line predicate 0
     in case result of
            Just a -> (a, n) : allPredicateMatches xs predicate (n + 1)
            Nothing -> allPredicateMatches xs predicate (n + 1)

matchesPredicate :: String -> (String -> Maybe a) -> Int -> Maybe a
matchesPredicate str predicate n =
    if n > length str
        then Nothing
        else
            let result = predicate $ take n str
             in case result of
                    Just a -> Just a
                    Nothing -> matchesPredicate str predicate (n + 1)

digitString :: String -> Maybe Int
digitString letters = case letters of
    "one" -> Just 1
    "two" -> Just 2
    "three" -> Just 3
    "four" -> Just 4
    "five" -> Just 5
    "six" -> Just 6
    "seven" -> Just 7
    "eight" -> Just 8
    "nine" -> Just 9
    _ -> Nothing
