module WinnerSearch where

import DecodeJsonMap
import Validation

countSize = 3
tableSize = 2

isGameOver :: String -> Bool
isGameOver msg = 
    let 
        moves = decode msg
    in checkColumns moves 0 0 || checkRows moves 0 0 || checkColumns moves 0 1 || checkRows moves 0 1 || checkDiagnoles moves || isLastMove moves

checkDiagnoles :: Moves -> Bool
checkDiagnoles moves = (checkDiagnole1 moves 0 0 == countSize) || (checkDiagnole1 moves 0 1 == countSize) || (checkDiagnole2 moves 0 0 == countSize) || (checkDiagnole2 moves 0 1 == countSize)


checkColumns :: Moves -> Int -> Int -> Bool
checkColumns moves num xy =
    let 
        check = checkColumn moves num 0 xy
        in if (num <= countSize && check /= countSize)
            then checkColumns moves (num + 1) xy
            else check == countSize

checkColumn :: Moves -> Int -> Int -> Int -> Int
checkColumn [] num sum xy = sum
checkColumn moves num sum xy =
    let
        id = read (fst (head moves)) :: Int
        movesTail = tail moves
        in if (snd (snd' (snd (head moves))) == (show num)) && ((mod id 2) == xy)
            then checkColumn (tail moves) num (sum + 1 ) xy
            else checkColumn (tail moves) num sum xy

checkRows :: Moves -> Int -> Int -> Bool
checkRows moves num xy =
    let 
        check = checkRow moves num 0 xy
        in if (num <= countSize && check /= countSize)
            then checkRows moves (num + 1) xy
            else check == countSize

checkRow :: Moves -> Int -> Int -> Int -> Int
checkRow [] num sum xy = sum
checkRow moves num sum xy =
    let
        id = read (fst (head moves)) :: Int
        movesTail = tail moves
        in if (snd (fst' (snd (head moves))) == (show num)) && ((mod id 2) == xy)
            then checkRow (tail moves) num (sum + 1) xy
            else checkRow (tail moves) num sum xy

checkDiagnole1 :: Moves -> Int -> Int -> Int
checkDiagnole1 [] sum xy = sum
checkDiagnole1 moves sum xy =
    let
        id = read (fst (head moves)) :: Int
        movesTail = tail moves
        in if (snd (fst' (snd (head moves))) == snd (snd' (snd (head moves)))) && ((mod id 2) == xy)
            then checkDiagnole1 (tail moves) (sum + 1) xy
            else checkDiagnole1 (tail moves) sum xy

checkDiagnole2 :: Moves -> Int -> Int -> Int
checkDiagnole2 [] sum xy = sum
checkDiagnole2 moves sum xy =
    let
        id = read (fst (head moves)) :: Int
        movesTail = tail moves
        x = read (snd (fst' (snd (head moves)))) :: Int
        y = read (snd (snd' (snd (head moves)))) :: Int
        in if (x + y == tableSize) && ((mod id 2) == xy)
            then checkDiagnole2 (tail moves) (sum + 1) xy
            else checkDiagnole2 (tail moves) sum xy