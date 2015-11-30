module Validation where

import Data.List
import DecodeJsonMap

data MarkType = Cross | Zero deriving (Eq)

validate :: String -> Bool
validate str = 
    let 
        moves = decode str
    in (validatePositions moves) && (validateOrder moves Cross)

validatePositions :: Moves -> Bool
validatePositions [] = True
validatePositions moves = 
    if validatePosition (tail moves) (head moves)
        then validatePositions (tail moves)
        else False

validatePosition :: Moves -> Move -> Bool
validatePosition [] move = True
validatePosition moves move =
    let
        moveInfos = snd move
        movesHead = snd (head moves)
        in if (snd (fst' moveInfos) == snd (fst' movesHead)) && (snd (snd' moveInfos) == snd (snd' movesHead))
            then False
            else validatePosition (tail moves) move

validateOrder :: Moves -> MarkType -> Bool
validateOrder [] markType = True
validateOrder moves markType = 
    let
        value = snd (thr' (snd (head moves)))
        in if ((value == "x" || value == "X") && markType == Cross) 
            then validateOrder (tail moves) Zero
            else if ((value == "o" || value == "O" || value == "0") && markType == Zero)
            then validateOrder (tail moves) Cross
            else False

fst' :: MoveInfos -> MoveInfo
fst' (a,_,_) = a

snd' :: MoveInfos -> MoveInfo
snd' (_,b,_) = b

thr' :: MoveInfos -> MoveInfo
thr' (_,_,c) = c

isLastMove :: Moves -> Bool
isLastMove moves = (length moves) == 9
