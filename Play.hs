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

play :: Game -> Result -> ComputerPlayer -> TournammentState -> IO TournammentState

play game start opponent tournament_state =
  let (wins, losses,ties) = tournament_state in
  do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      line <- getLine
      if line == "0"
        then
            person_play game start opponent tournament_state
        else if line ==  "1"
        then computer_play game start opponent tournament_state
        else if line == "2"
            then return tournament_state
        else play game start opponent tournament_state

person_play :: Game -> Result -> ComputerPlayer -> TournammentState -> IO TournammentState
-- opponent has played, the person must now play
person_play game (EndOfGame 'O' start_state) opponent (wins,losses,ties) =
   do
      putStrLn "Computer won!"
      play game (ContinueGame start_state) opponent (wins,losses+1,ties)
person_play game (EndOfGame 't' start_state) opponent (wins,losses,ties) =
   do
      putStrLn "It's a draw"
      play game (ContinueGame start_state) opponent (wins,losses,ties+1)
person_play game (ContinueGame state) opponent tournament_state =
   do
      let State gameBoard colCount = state
      putStrLn "Choose column 0-6 to place peice"
      putStrLn (printArray gameBoard)
      line <- getLine
      let action = (readMaybe line :: Maybe Int)
      if (action == Nothing) || (fromJust action) > length(colCount) || (colCount !! (fromJust action)) < 0
        then  -- error; redo
           person_play game (ContinueGame state) opponent tournament_state
        else
           computer_play game (game 'X' ((fromJust action)) state) opponent tournament_state


computer_play :: Game -> Result -> ComputerPlayer -> TournammentState -> IO TournammentState
-- computer_play game current_result opponent tournament_state
-- person has played, the computer must now play
computer_play game (EndOfGame 'X'  start_state) opponent (wins,losses,ties) =
   do
      putStrLn "You won!"
      play game  (ContinueGame start_state) opponent (wins+1,losses,ties)
computer_play game (EndOfGame 't' start_state) opponent (wins,losses,ties) =
   do
      putStrLn "I't a draw"
      play game (ContinueGame start_state) opponent (wins,losses,ties+1)
      
computer_play game (ContinueGame state) opponent tournament_state =
      let 
          opponent_move = opponent state
        in
          do
            putStrLn ("The computer chose "++show opponent_move)
            person_play game (game 'O' opponent_move state) opponent tournament_state

-- play connect4 (ContinueGame connect4_start) simple_player (0,0,0)

