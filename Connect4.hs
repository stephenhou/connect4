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

type Game = Player -> Action -> State -> Result

type ColCount = [Int]

type Player = Char

--type Player = State -> Action

------ The Connect4 Sum Game -------

newtype Action = Action Int                          -- a move for a player
         deriving (Ord,Eq)

newtype Row = Row [Char]                          -- a move for a player
         deriving (Ord,Eq, Show)

data MoveRes = MoveRes GameBoard (Int, Int)
type GameBoard = [Row]   -- (self,other)

instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]

connect4 :: Game
connect4 player (Action move) (State board colPos)
    | win (MoveRes board (colPos !! move, move))  player = EndOfGame player connect4_start   -- agent wins
    | isBoardFull board                                  = EndOfGame 't'    connect4_start     -- no more moves, draw
    | otherwise                                          =
          ContinueGame (State newBoard newColCount)
    where (State newBoard newColCount) = updateBoard player move (State board colPos)

updateBoard :: Player -> Int -> State -> State
updateBoard player x (State board colPos) =
    let y = colPos !! x
        (Row rowToUpdate) = board !! x
        updatedColCount = replaceNth x (y-1) colPos
        updatedRow = Row (replaceNth x player rowToUpdate)
        updatedBoard = replaceNth y updatedRow board
    in (State updatedBoard updatedColCount)

isBoardFull :: GameBoard -> Bool
isBoardFull [] = False
isBoardFull ((Row r) : rest) = rowHasSpot(r) || isBoardFull(rest)

rowHasSpot :: [Char] -> Bool
rowHasSpot [] = True
rowHasSpot (p : rest)
    | p == '*' = False
    | otherwise = rowHasSpot(rest)

replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs


connect4_start = State [Row ['*', '*', '*', '*', '*', '*', '*'], 
                        Row ['*', '*', '*', '*', '*', '*', '*'],
                        Row ['*', '*', '*', '*', '*', '*', '*'],
                        Row ['*', '*', '*', '*', '*', '*', '*'],
                        Row ['*', '*', '*', '*', '*', '*', '*'],
                        Row ['*', '*', '*', '*', '*', '*', '*']] [5,5,5,5,5,5,5]

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
    where (Row row) = board !! i


-- win tests
false_start = MoveRes [Row ['*', '*', '*', '*', '*', '*', '*'], 
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['*', '*', '*', '*', '*', '*', '*']] (5, 0)

false_oneoff = MoveRes [Row ['*', '*', '*', '*', '*', '*', '*'], 
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['X', 'X', 'X', '*', '*', '*', '*'],
    Row ['X', 'X', 'X', '*', '*', '*', '*'],
    Row ['X', 'X', 'X', '*', '*', '*', '*']] (5, 0)

false_blocked = MoveRes [Row ['*', '*', '*', '*', '*', '*', '*'], 
     Row ['*', 'O', 'O', 'O', 'O', 'O', '*'],
     Row ['*', 'O', 'X', 'X', 'X', 'O', '*'],
     Row ['*', 'O', 'X', 'X', 'X', 'O', '*'],
     Row ['*', 'O', 'X', 'x', 'X', 'O', '*'],
     Row ['*', 'O', 'O', 'O', 'O', 'O', '*']] (1, 3)

true_vert = MoveRes [Row ['*', '*', '*', '*', '*', '*', '*'], 
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['*', '*', '*', 'X', '*', '*', '*'],
    Row ['*', '*', '*', 'X', '*', '*', '*'],
    Row ['*', '*', '*', 'X', '*', '*', '*'],
    Row ['*', '*', '*', 'X', '*', '*', '*']] (2, 3)
    
true_horz = MoveRes [Row ['*', '*', '*', '*', '*', '*', '*'], 
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['*', '*', 'X', 'X', 'X', 'X', '*']] (5, 4)

true_diag_down = MoveRes [Row ['*', '*', '*', '*', '*', '*', '*'], 
    Row ['*', '*', '*', '*', '*', '*', '*'],
    Row ['X', '*', '*', '*', '*', '*', '*'],
    Row ['*', 'X', '*', '*', '*', '*', '*'],
    Row ['*', '*', 'X', '*', '*', '*', '*'],
    Row ['*', '*', '*', 'X', '*', '*', '*']] (2, 0)

true_diag_up = MoveRes [Row ['*', '*', '*', '*', '*', '*', '*'], 
    Row ['*', '*', '*', '*', 'X', '*', '*'],
    Row ['*', '*', '*', 'X', '*', '*', '*'],
    Row ['*', '*', 'X', '*', '*', '*', '*'],
    Row ['*', 'X', '*', '*', '*', '*', '*'],
    Row ['*', '*', '*', '*', '*', '*', '*']] (2, 3)
