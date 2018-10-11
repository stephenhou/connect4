-- CPSC 312 - 2018 - Games in Haskell
module Connect4 where

-- To run it, try:
-- ghci
-- :load Connect4

data State = State GameBoard [Int]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Double State    -- end of game, value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq)

type Game = Action -> State -> Result

type Player = State -> Action

------ The Connect4 Sum Game -------

newtype Action = Action Int                          -- a move for a player
         deriving (Ord,Eq)

newtype Row = Row [Char]                          -- a move for a player
         deriving (Ord,Eq, Show)

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