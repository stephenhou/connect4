-- CPSC 312 - 2018 - Games in Haskell
module Play where

-- To run it, try:
-- ghci
-- :load Play

import Connect4
import System.IO
import Text.Read   (readMaybe)
import Data.Maybe   (fromJust)

type TournammentState = (Int,Int,Int)   -- wins, losses, ties

--function to print our shit
printArray arr =
  unlines [unwords [show (arr !! y !! x) | x <- [0..6]] | y <- [0..5]]

play :: Game -> Result -> TournammentState -> Opponent -> IO TournammentState

play game start tournament_state opponent =
  let (wins, losses,ties) = tournament_state in
  do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      line <- getLine
      if line == "0"
        then
            person_play game start tournament_state opponent
        else if line ==  "1"
        then computer_play game start tournament_state (-1) opponent
        else if line == "2"
            then return tournament_state
        else play game start tournament_state opponent

person_play :: Game -> Result -> TournammentState -> Opponent -> IO TournammentState
-- opponent has played, the person must now play
person_play game (EndOfGame 'Y' start_state) (wins,losses,ties) opponent =
   do
      putStrLn "Computer won!"
      play game (ContinueGame start_state) (wins,losses+1,ties) opponent
person_play game (EndOfGame 't' start_state) (wins,losses,ties) opponent =
   do
      putStrLn "It's a draw"
      play game (ContinueGame start_state) (wins,losses,ties+1) opponent
person_play game (ContinueGame state) tournament_state opponent =
   do
      let State gameBoard colCount = state
      putStrLn "Choose column 1-7 to place piece"
      putStrLn (printArray gameBoard)
      line <- getLine
      let action = (readMaybe line :: Maybe Int)
      if (action == Nothing) || (fromJust action) -1 > length(colCount) - 1 || (colCount !! (fromJust action) -1) < 0
        then  -- error; redo
           person_play game (ContinueGame state) tournament_state opponent
        else
           computer_play game (game 'X' ((fromJust action) -1) state) tournament_state ((fromJust action) -1) opponent

computer_play :: Game -> Result -> TournammentState -> Int -> Opponent-> IO TournammentState
-- person has played, the computer must now play
computer_play game (EndOfGame 'X'  start_state) (wins,losses,ties) player_move opponent =
  do
      putStrLn "You won!"
      play game  (ContinueGame start_state) (wins+1,losses,ties) opponent
computer_play game (EndOfGame 't' start_state) (wins,losses,ties) player_move opponent =
  do
      putStrLn "I't a draw"
      play game (ContinueGame start_state) (wins,losses,ties+1) opponent
      
-- TODO: make sure to handle case where player_move is -1
computer_play game (ContinueGame state) tournament_state player_move opponent =
      let 
          opponent_move = computer state player_move
        in
          do
            putStrLn ("The computer chose "++show opponent_move)
            person_play game (game 'Y' opponent_move state) tournament_state opponent

-- play connect4 (ContinueGame connect4_start) (0,0,0) computer
