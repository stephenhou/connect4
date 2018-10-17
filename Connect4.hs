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
type Opponent = State -> Int -> Int -> Int

type ComputerPlayer = State -> Int

--type Player = State -> Action

------ The Connect4 Sum Game -------

data MoveRes = MoveRes GameBoard (Int, Int)
type GameBoard = [[Char]]   -- (self,other)

connect4 :: Game
connect4 player  move (State board colPos)
    | win (MoveRes newBoard (colPos !! move, move))  player = EndOfGame player connect4_start   -- agent wins
    | isBoardFull newColCount                                  = EndOfGame 't'    connect4_start     -- no more moves, draw
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

isBoardFull :: [Int] -> Bool
isBoardFull [] = True
isBoardFull (r : rest) = if r > 0 then False else isBoardFull(rest)

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

-- checks for num consecutive elements of player, the functions passed in are used to move along horizontally/vertically/diagonally
checkConsecutive :: GameBoard -> Char -> Int -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Bool
checkConsecutive board player num x y fi fj gi gj = 
    ((checkHelper board player x y fi fj (-1)) + (checkHelper board player x y gi gj (-1))) >= (num-1)

checkHelper :: GameBoard -> Char -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Int -> Int
checkHelper board player i j fi fj acc
    | (isOutOfBound i j) || (row !! j /= player) = acc
    | otherwise = checkHelper board player (fi i) (fj j) fi fj (acc+1) 
    where row = board !! i


isOutOfBound :: Int -> Int -> Bool
isOutOfBound i j = i > 5 || j > 6 || i < 0 || j < 0

------- Computer Opponent -------

computer :: Opponent
-- this player has an ordering of the moves, and chooses the first one available
computer (State gb cols) playerMove prevMove = 
    let offset = if prevMove == playerMove then 1 else 0
        winningMove = if prevMove /= -1 then findMandatoryMove (State gb cols) prevMove 'O' offset else (-1)
        nextMove = if playerMove /= -1 then findMandatoryMove (State gb cols) playerMove 'X' 0 else (-1)
        compMove = computerMove cols playerMove prevMove
    in if winningMove /= -1 then winningMove
         else if nextMove /= -1 then nextMove
         else if compMove < 0 then 0
         else if compMove > 6 then 6 
         else if (cols !! compMove) < 0 then findOpenSpot cols 0 else compMove

computerMove :: [Int] -> Int -> Int -> Int
-- this handles the more complex move finding for the computer
computerMove cols playerMove prevMove = 
    if playerMove == -1 
        then 3
        else if (cols !! (playerMove)) -1  == (cols !! (max 0 (playerMove -1))) && (cols !! (max 0 (playerMove -1))) == (cols !! (min 6 (playerMove  + 1)))
                then playerMove
                else if (cols !! (max 0 (playerMove - 1))) > (cols !! (min 6 (playerMove + 1)))
                    then playerMove - 1
                    else playerMove + 1

findOpenSpot (h:t) counter = 
    if h > 0 then counter else findOpenSpot t (counter +1)

-- can be used to find the winning move for the computer
-- OR
-- find the position to block to prevent human from winning
findMandatoryMove :: State -> Int -> Char -> Int -> Int
findMandatoryMove (State board colPos) playerMove player offset = 
    let horzCheckStr = (checkStringHelper (State board colPos) 0 (((colPos !! playerMove)+1)+offset) (playerMove+3) (\ i -> i) (\ j -> j-1) [])
        diagDownCheckStr = (checkStringHelper (State board colPos) 0 (((colPos !! playerMove)+4)+offset) (playerMove+3) (\ i -> i-1) (\ j -> j-1) [])
        diagUpCheckStr = (checkStringHelper (State board colPos) 0 (((colPos !! playerMove)-2)+offset) (playerMove+3) (\ i -> i+1) (\ j -> j-1) [])
        horzPos = (findWinningGap player horzCheckStr playerMove 0 (length horzCheckStr) 0)
        diagDownPos = (findWinningGap player diagDownCheckStr playerMove 0 (length diagDownCheckStr) 0)
        diagUpPos = (findWinningGap player diagUpCheckStr playerMove 0 (length diagUpCheckStr) 0)
    in if horzPos /= -1 then horzPos
         else if diagDownPos /= -1 then diagDownPos
         else if diagUpPos /= -1 then diagUpPos
         else if (vertWin (State board colPos) playerMove player) && (offset == 0) then playerMove
         else -1


-- identifies from the human's last move the vertical/horizontal/diagonal section that could now be 1 move away from winning
checkStringHelper :: State -> Int -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> [Char] -> [Char]
checkStringHelper (State board colPos) count i j fi fj acc
    | (count == 7) = acc
    | isOutOfBound i j = checkStringHelper (State board colPos) (count+1) (fi i) (fj j) fi fj (['@'] ++ acc) -- signals out of bounds
    | otherwise = checkStringHelper (State board colPos) (count+1) (fi i) (fj j) fi fj (elt ++ acc)
    where elt = if ((colPos !! j) <= i) then [(board !! i !! j)] else ['#'] -- if this spot isn't actually reachable on the next move, disregard

-- uses check string to find a gap that could win the game for the input player
findWinningGap :: Char -> [Char] -> Int -> Int -> Int -> Int -> Int
findWinningGap player checkStr realPos pos len prevMatches 
    | (pos == len) = -1
    | ((checkStr !! pos) == player) = findWinningGap player checkStr realPos (pos+1) len (prevMatches+1)
    | ((checkStr !! pos) == '*') = if (checkNextInCheckStr player checkStr (pos+1) len (3-prevMatches)) then (realPos + (pos-3)) else findWinningGap player checkStr realPos (pos+1) len 0
    | otherwise = findWinningGap player checkStr realPos (pos+1) len 0

checkNextInCheckStr :: Char -> [Char] -> Int -> Int -> Int -> Bool
checkNextInCheckStr player checkStr pos len checksLeft
    | (checksLeft == 0) = True
    | (pos == len) = False
    | otherwise = currCheck && checkNextInCheckStr player checkStr (pos+1) len (checksLeft-1)
    where currCheck = ((checkStr !! pos) == player)

vertWin :: State -> Int -> Char -> Bool
vertWin (State board colPos) playerMove player = 
    let lastRow = ((colPos !! playerMove)+1)
    in if lastRow < 4 then
        (board !! (lastRow+1) !! playerMove) == player &&
        (board !! (lastRow+2) !! playerMove) == player
    else False

test_board = [ ['*', '*', '*', '*', '*', '*', '*'], 
    ['*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*'],
    ['X', 'X', 'X', '*', '*', '*', '*']]

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


