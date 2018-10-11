-- CPSC 312 - 2018 - Games in Haskell
module Connect4 where

-- To run it, try:
-- ghci
-- :load Connect4

data State = State GameBoard [Int]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Double State    -- end of game, value, starting state
            | ContinueGame State         -- continue with new state
         deriving (Eq)

type Game = Action -> State -> Result

type Player = State -> Action

------ The Connect4 Sum Game -------

newtype Action = Action Int                          -- a move for a player
         deriving (Ord,Eq)

newtype Row = Row [Char]                          -- a move for a player
         deriving (Ord,Eq, Show)

newtype MoveRes = MoveRes (GameBoard, (Int, Int))

type GameBoard = [Row, Row, Row, Row, Row, Row, Row]   -- (self,other)

instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]


Connect4 :: Game
Connect4 move (State board colPos)
    | win move mine                = EndOfGame 1 connect4_start   -- agent wins
    | available == [move]          = EndOfGame 0 connect4_start     -- no more moves, draw
    | otherwise                    =
          ContinueGame (State (others,(move:mine))   -- note roles have flipped
                        [act | act <- available, act /= move])

connect4_start = State [Row ['*', '*', '*', '*', '*', '*', '*'], 
                        Row ['*', '*', '*', '*', '*', '*', '*'],
                        Row ['*', '*', '*', '*', '*', '*', '*'],
                        Row ['*', '*', '*', '*', '*', '*', '*'],
                        Row ['*', '*', '*', '*', '*', '*', '*'],
                        Row ['*', '*', '*', '*', '*', '*', '*']] [0,0,0,0,0,0,0]

win :: MoveRes Char -> Bool
win (board, (i, j)) player =
    (checkConsecutive board player 4 i j (\ i -> i) (\ j -> j+1) (\ i -> i) (\j -> j-1)) ||     -- check horizontal
    (checkConsecutive board player 4 i j (\ i -> i+1) (\ j -> j) (\ i -> i-1) (\j -> j)) ||     -- check vertical
    (checkConsecutive board player 4 i j (\ i -> i-1) (\ j -> j-1) (\ i -> i+1) (\j -> j+1)) || -- check downwards diagonal
    (checkConsecutive board player 4 i j (\ i -> i+1) (\ j -> j-1) (\ i -> i-1) (\j -> j+1))    -- check upwards diagonal

checkConsecutive :: GameBoard -> Char -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Bool
checkConsecutive player num x y fi fj gi gj = 
    checkHelper board player x y fi fj 0) + (checkHelper board player num gi gj 0) > (num-1)

checkHelper :: GameBoard -> Char -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Int -> Int
checkHelper board player i j fi fj acc
    | i > 5 || j > 6 || i < 0 || j < 0 || (board !! i !! j) != player = acc
    | otherwise = checkHelper board player (fi i) (fj j) (acc+1) 


------- A Player -------

--simple_player :: Player
-- this player has an ordering of the moves, and chooses the first one available
--simple_player (State _ avail) = head [Action e | e <- [5,6,4,2,8,1,3,7,9],
--                                               Action e `elem` avail]


-- Test cases
-- magicsum magicsum_start (simple_player magicsum_start)
-- a i = Action i  -- make it easier to type
-- as lst = [Action i | i <- lst]
-- magicsum (a 6) (State (as [3,5], as [2,7]) (as [1,4,6,8,9])) 
-- magicsum (a 3) (State (as [5,7], as [2,9]) (as [1,3,4,6,8])) 






-- Why is it called the "magic sum game"?
-- The following is a magic square:
-- 294
-- 753
-- 618