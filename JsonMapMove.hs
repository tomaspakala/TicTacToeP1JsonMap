module JsonMapMove where

import Text.Format
import DecodeJsonMap
import Validation

lastMoveTemplate = ", \"{0}\": {\"x\": {1}, \"y\": {2}, \"v\": \"x\"}}"

firstMoveTemplate = "{\"{0}\": {\"x\": {1}, \"y\": {2}, \"v\": \"x\"}}"

tableSize = 2

addMoves :: String -> String
addMoves msg =
    if msg == ""
        then encodeJsonMapMove firstMoveTemplate 0 1 1
        else lastMove msg

lastMove :: String -> String
lastMove msg =
    let
        moves = decode msg
        n = length moves
        xy = getMove moves 0 0
        msg' = take (length msg - 1) msg
    in msg' ++ encodeJsonMapMove lastMoveTemplate n (fst xy) (snd xy)

encodeJsonMapMove :: String -> Int -> Int -> Int -> String
encodeJsonMapMove template pos x y = format template [show pos, show x, show y]

getMove :: Moves -> Int -> Int -> (Int, Int)
getMove moves x y = 
    let
        isExist = checkMove moves x y
        in if (isExist == False)
            then (x, y)
            else if (x < tableSize)
                then getMove moves (x + 1) y
            else if (y < tableSize && x == tableSize)
                then getMove moves 0 (y + 1)
            else if (y < tableSize)
                then getMove moves x (y + 1)
            else (0, 0)

checkMove :: Moves -> Int -> Int -> Bool
checkMove [] x y = False
checkMove moves x y = 
    if (snd (fst'(snd (head moves))) == show x) && (snd (snd'(snd (head moves))) == show y)
        then True
        else checkMove (tail moves) x y