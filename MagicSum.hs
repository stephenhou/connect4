-- CPSC 312 - 2018 - Games in Haskell
module MagicSum where

-- To run it, try:
-- ghci
-- :load MagicSum

data State = State InternalState  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Double State    -- end of game, value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

------ The Magic Sum Game -------

newtype Col = Col [Char]                          -- a move for a player
         deriving (Ord,Eq)
type InternalState = ([Col],[Col],[Col],[Col],[Col],[Col],[Col])


magicsum_start = State (['*', '*', '*', '*', '*', '*'],['*', '*', '*', '*', '*', '*'],['*', '*', '*', '*', '*', '*'],['*', '*', '*', '*', '*', '*'],['*', '*', '*', '*', '*', '*'],['*', '*', '*', '*', '*', '*'],['*', '*', '*', '*', '*', '*'])

-- win n ns = the agent wins if it selects n given it has already selected ns


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