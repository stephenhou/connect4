-- CPSC 312 - 2018 - Games in Haskell
module Connect4 where

-- To run it, try:
-- ghci
-- :load Connect4

data State = State GameBoard [Int]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Char State    -- end of game, value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq)

type Game = Player -> Int -> State -> Result

type Player = Char

type ComputerPlayer = State -> Int

--type Player = State -> Action

------ The Connect4 Sum Game -------

data MoveRes = MoveRes GameBoard (Int, Int)
type GameBoard = [[Char]]   -- (self,other)

connect4 :: Game
connect4 player  move (State board colPos)
    | win (MoveRes newBoard (colPos !! move, move))  player = EndOfGame player connect4_start   -- agent wins
    | isBoardFull newBoard                                  = EndOfGame 't'    connect4_start     -- no more moves, draw
    | otherwise                                          =
          ContinueGame (State newBoard newColCount)
    where (State newBoard newColCount) = updateBoard player move (State board colPos)

updateBoard :: Player -> Int -> State -> State
updateBoard player x (State board colPos) =
    let y = colPos !! x
        rowToUpdate = board !! y
        updatedColCount = replaceNth x (y-1) colPos
        updatedRow = replaceNth x player rowToUpdate
        updatedBoard = replaceNth y updatedRow board
    in (State updatedBoard updatedColCount)

isBoardFull :: GameBoard -> Bool
isBoardFull [] = False
isBoardFull (r : rest) = rowHasSpot(r) || isBoardFull(rest)

rowHasSpot :: [Char] -> Bool
rowHasSpot [] = True
rowHasSpot (p : rest)
    | p == '*' = False
    | otherwise = rowHasSpot(rest)

replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs


connect4_start = State [['*', '*', '*', '*', '*', '*', '*'], 
                        ['*', '*', '*', '*', '*', '*', '*'],
                        ['*', '*', '*', '*', '*', '*', '*'],
                        ['*', '*', '*', '*', '*', '*', '*'],
                        ['*', '*', '*', '*', '*', '*', '*'],
                        ['*', '*', '*', '*', '*', '*', '*']] [5,5,5,5,5,5,5]

win :: MoveRes -> Char -> Bool
win (MoveRes board (i, j)) player =
    (checkConsecutive board player 4 i j (\ i -> i) (\ j -> j+1) (\ i -> i) (\j -> j-1)) ||     -- check horizontal
    (checkConsecutive board player 4 i j (\ i -> i+1) (\ j -> j) (\ i -> i-1) (\j -> j)) ||     -- check vertical
    (checkConsecutive board player 4 i j (\ i -> i-1) (\ j -> j-1) (\ i -> i+1) (\j -> j+1)) || -- check downwards diagonal
    (checkConsecutive board player 4 i j (\ i -> i+1) (\ j -> j-1) (\ i -> i-1) (\j -> j+1))    -- check upwards diagonal

checkConsecutive :: GameBoard -> Char -> Int -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Bool
checkConsecutive board player num x y fi fj gi gj = 
    ((checkHelper board player x y fi fj (-1)) + (checkHelper board player x y gi gj (-1))) >= (num-1)

checkHelper :: GameBoard -> Char -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Int -> Int
checkHelper board player i j fi fj acc
    | i > 5 || j > 6 || i < 0 || j < 0 || (row !! j /= player) = acc
    | otherwise = checkHelper board player (fi i) (fj j) fi fj (acc+1) 
    where row = board !! i

simple_player :: ComputerPlayer
simple_player state = 6

-- win tests
false_start = MoveRes [ ['*', '*', '*', '*', '*', '*', '*'], 
     ['*', '*', '*', '*', '*', '*', '*'],
     ['*', '*', '*', '*', '*', '*', '*'],
     ['*', '*', '*', '*', '*', '*', '*'],
     ['*', '*', '*', '*', '*', '*', '*'],
     ['*', '*', '*', '*', '*', '*', '*']] (5, 0)

false_oneoff = MoveRes [ ['*', '*', '*', '*', '*', '*', '*'], 
     ['*', '*', '*', '*', '*', '*', '*'],
     ['*', '*', '*', '*', '*', '*', '*'],
     ['X', 'X', 'X', '*', '*', '*', '*'],
     ['X', 'X', 'X', '*', '*', '*', '*'],
     ['X', 'X', 'X', '*', '*', '*', '*']] (5, 0)

false_blocked = MoveRes [ ['*', '*', '*', '*', '*', '*', '*'], 
      ['*', 'O', 'O', 'O', 'O', 'O', '*'],
      ['*', 'O', 'X', 'X', 'X', 'O', '*'],
      ['*', 'O', 'X', 'X', 'X', 'O', '*'],
      ['*', 'O', 'X', 'x', 'X', 'O', '*'],
      ['*', 'O', 'O', 'O', 'O', 'O', '*']] (1, 3)

true_vert = MoveRes [ ['*', '*', '*', '*', '*', '*', '*'], 
     ['*', '*', '*', '*', '*', '*', '*'],
     ['*', '*', '*', 'X', '*', '*', '*'],
     ['*', '*', '*', 'X', '*', '*', '*'],
     ['*', '*', '*', 'X', '*', '*', '*'],
     ['*', '*', '*', 'X', '*', '*', '*']] (2, 3)
    
true_horz = MoveRes [ ['*', '*', '*', '*', '*', '*', '*'], 
     ['*', '*', '*', '*', '*', '*', '*'],
     ['*', '*', '*', '*', '*', '*', '*'],
     ['*', '*', '*', '*', '*', '*', '*'],
     ['*', '*', '*', '*', '*', '*', '*'],
     ['*', '*', 'X', 'X', 'X', 'X', '*']] (5, 4)

true_diag_down = MoveRes [ ['*', '*', '*', '*', '*', '*', '*'], 
     ['*', '*', '*', '*', '*', '*', '*'],
     ['X', '*', '*', '*', '*', '*', '*'],
     ['*', 'X', '*', '*', '*', '*', '*'],
     ['*', '*', 'X', '*', '*', '*', '*'],
     ['*', '*', '*', 'X', '*', '*', '*']] (2, 0)

true_diag_up = MoveRes [ ['*', '*', '*', '*', '*', '*', '*'], 
     ['*', '*', '*', '*', 'X', '*', '*'],
     ['*', '*', '*', 'X', '*', '*', '*'],
     ['*', '*', 'X', '*', '*', '*', '*'],
     ['*', 'X', '*', '*', '*', '*', '*'],
     ['*', '*', '*', '*', '*', '*', '*']] (2, 3)
