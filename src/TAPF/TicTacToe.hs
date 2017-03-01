{-|
  Game representation and moves for Tic-Tac-Toe.
  Pedro Vasconcelos, 2016
-}
module TAPF.TicTacToe where

import           Data.Map (Map)
import qualified Data.Map as Map

-- | the two players
data Player = X | O deriving (Eq, Show)

-- | row, column coordinates
type Coord = (Int,Int)

-- | game states
data State = Playing Player   -- next to play
           | Winner Player    -- game ended with a winner
           | Draw             -- game ended with a draw
           deriving (Eq, Show)

-- | the game board
-- mapping coordinates to players
type Board = Map Coord Player

-- | a complete game position
data TicTacToe =
  TicTacToe { state :: State
            , board :: Board
            } deriving Show


-- | switch to other player
invert :: Player -> Player
invert X = O
invert O = X


-- | game board size (constant)
boardSize :: Int
boardSize = 3

-- | initial game position
start :: Player -> TicTacToe
start player = TicTacToe { state = Playing player
                         , board = Map.empty }

-- | check the winning condition for a board
winner :: Board -> Bool
winner board = any checkWin (rows ++ cols ++ diags)
  where row i = [Map.lookup (i,j) board | j<-[1..boardSize]]
        col j = [Map.lookup (i,j) board | i<-[1..boardSize]]
        diag1 = [Map.lookup (i,i) board | i<-[1..boardSize]]
        diag2 = [Map.lookup (i,boardSize+1-i) board | i<-[1..boardSize]]
        rows = map row [1..boardSize]
        cols = map col [1..boardSize]
        diags = [diag1, diag2]


-- | check the winning condition for a line
checkWin :: [Maybe Player] -> Bool
checkWin (first@(Just p):rest) = all (==first) rest
checkWin _  = False


-- | list of available coordinates in a board
freeCoordinates :: Board -> [Coord]
freeCoordinates board
  = [(i,j) |  i<-[1..boardSize], j<-[1..boardSize],
     (i,j) `Map.notMember` board]

-- | list possible moves from a position
availableMoves :: TicTacToe -> [Coord]
availableMoves g
  = case state g of
    Playing player -> freeCoordinates (board g)
    _ -> []   -- game has ended; no further moves


-- | play a valid move
makeMove :: Coord -> TicTacToe -> TicTacToe
makeMove coord g
  = case state g of
  Playing player ->
    let board' = Map.insert coord player (board g)
    in TicTacToe { state = nextState player board', board=board' }
  _ -> g  -- game has ended; no further moves

-- | helper function; advance to next state
nextState :: Player -> Board -> State
nextState player board
  | winner board  = Winner player
  | null (freeCoordinates board) = Draw
  | otherwise     = Playing (invert player)
