module TicTacToe where

-- Main logic of the tictactoe game

data Player = PX | PO deriving (Eq)

data CellData = X | O | E deriving (Eq, Show, Read)

type GameState = [CellData]

data GameStatus = Oturn | Xturn | Xwin | Owin | Draw deriving (Eq, Show, Read)

initialState :: GameState
initialState = replicate 9 E

-- testState :: GameState
-- testState = [O,O,E,O,X,E,X,E,E]

check :: GameState -> CellData -> Int -> Int -> Bool
check s cd y x = s !! ind == cd
  where
    ind = y * 3 + x

hasWinPattern :: GameState -> Player -> Bool
hasWinPattern s p = hasH || hasV || hasD1 || hasD2
  where
    hasH = any hasHoriz [0 .. 2]
    hasHoriz y = all (check s cd y) [0 .. 2]
    hasV = any hasVertic [0 .. 2]
    hasVertic x = all (\y -> check s cd y x) [0 .. 2]
    hasD1 = all (\xy -> check s cd xy xy) [0 .. 2]
    cd = if p == PX then X else O
    hasD2 = all (uncurry $ check s cd) [(0, 2), (1, 1), (2, 0)]

gameStatus :: GameState -> GameStatus
gameStatus s
  | hasWinPattern s PX = Xwin
  | hasWinPattern s PO = Owin
  | (cx + co) >= 9 = Draw
  | cx <= co = Xturn
  | otherwise = Oturn
  where
    co = Prelude.length [c | c <- s, c == O]
    cx = Prelude.length [c | c <- s, c == X]

isEmpty :: GameState -> Int -> Bool
isEmpty s i = s !! i == E

setCell :: GameState -> Int -> CellData -> GameState
setCell s i d
  | (0 <= i) && (i < Prelude.length s) = Prelude.take i s ++ d : Prelude.drop (i + 1) s
  | otherwise = error $ "unbound index, valid range from 0 to " ++ show (Prelude.length s - 1)

nextState :: GameState -> Int -> Maybe GameState
nextState s index
  | isEmpty s index = setCell s index <$> nextTurn
  | otherwise = Nothing
  where
    nextTurn
      | gameStatus s == Xturn = Just X
      | gameStatus s == Oturn = Just O
      | otherwise = Nothing
