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

type GameBoard = [Row]   -- (self,other)

instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]

connect4 :: Game
connect4 player move (State board colPos)
    | win move mine                = EndOfGame player    magicsum_start   -- agent wins
    | isBoardFull                  = EndOfGame 't'       magicsum_start     -- no more moves, draw
    | otherwise                    =
          ContinueGame (State newBoard newColCount)
    where (State newBoard newColCount) = updateBoard player move board colPos

updateBoard :: Player -> Action -> State -> State
updateBoard player (Action x) (State board colPos) =
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

-- win n ns = the agent wins if it selects n given it has already selected ns
--win :: Action -> [Action] -> Bool
--win (Action n) ns  = or [n+x+y==15 | Action x <- ns, Action y <- ns, x/=y]

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