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

play :: Game -> Result -> TournammentState -> IO TournammentState

play game start tournament_state =
  let (wins, losses,ties) = tournament_state in
  do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      line <- getLine
      if line == "0"
        then
            person_play game start tournament_state
        else if line ==  "1"
        then person_play game start tournament_state
        else if line == "2"
            then return tournament_state
        else play game start tournament_state

person_play :: Game -> Result -> TournammentState -> IO TournammentState
-- opponent has played, the person must now play
person_play game (EndOfGame 'X' start_state) (wins,losses,ties) =
   do
      putStrLn "Computer won!"
      play game (ContinueGame start_state) (wins,losses+1,ties)
person_play game (EndOfGame 'O' start_state) (wins,losses,ties) =
   do
      putStrLn "It's a draw"
      play game (ContinueGame start_state) (wins,losses,ties+1)
person_play game (ContinueGame state) tournament_state =
   do
      let State gameBoard colCount = state
      putStrLn "Choose column 1-7 to place peice"
      putStrLn (printArray gameBoard)
      line <- getLine
      let action = (readMaybe line :: Maybe Int)
      if (action == Nothing) || (fromJust action) -1 > length(colCount) - 1 || (colCount !! (fromJust action) -1) < 0
        then  -- error; redo
           person_play game (ContinueGame state) tournament_state
        else
           person_play game (game 'X' ((fromJust action) -1) state) tournament_state


-- play connect4 (ContinueGame connect4_start) (0,0,0)