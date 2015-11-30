module DecodeJsonMap where

import Data.List

type MoveInfo = (String, String)
type MoveInfos = (MoveInfo, MoveInfo, MoveInfo)
type Move = (String, MoveInfos)
type Moves = [Move]

decode :: String -> Moves
decode str = sortOn fst (readMoves (drop (1) (removeSpaces str)) [])

removeSpaces :: String -> String
removeSpaces str = filter(/=' ') str

readMoves :: String -> Moves -> Moves
readMoves [] acc = acc
readMoves str acc =
    let
        (move, rest) = readMove str
    in readMoves rest (move : acc)

readMove :: String -> (Move, String)
readMove move = 
    let
        key = removeExtraSymbols (takeWhile (/= ':') move) 1 1
        withoutKey = drop (length key + 3) move
        values = takeWhile (/= '}') withoutKey
        rest = drop (length key + length values + 5) move
        xCoordinate = readCoordinate values
        yCoordinate = readCoordinate (drop (length xCoordinate + 5) values)
        value = readMoveValue (drop (length xCoordinate + length yCoordinate + 10) values)
    in ((key, (xCoordinate, yCoordinate, value)), rest)

readMoveValue :: String -> MoveInfo
readMoveValue pair =
    let 
        key = removeExtraSymbols (takeWhile (/= ':') pair) 2 1
        withoutKey = drop (length key + 3) pair
        rest = removeExtraSymbols (takeWhile (/= ',') withoutKey) 2 1
    in (key, rest)

readCoordinate :: String -> MoveInfo
readCoordinate pair =
    let 
        key = removeExtraSymbols (takeWhile (/= ':') pair) 2 1
        withoutKey = drop (length key + 4) pair
        rest = takeWhile (/= ',') withoutKey
    in (key, rest)

removeExtraSymbols :: String -> Int -> Int-> String
removeExtraSymbols symbols dropStart dropEnd=
    let
        withoutBegining = drop (dropStart) symbols
        finished = take (length withoutBegining - dropEnd) withoutBegining
    in finished