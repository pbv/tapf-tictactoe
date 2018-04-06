{-
  Game representation for Tic-Tac-Toe.
  Pedro Vasconcelos, 2016-2018
-}
module TAPF.TicTacToe where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad (msum)

-- | the two players
data Player
  = X | O deriving (Eq, Show)

-- | row, column coordinates
type Coord = (Int,Int)

-- | possible game states
data State
  = Playing Player   -- ^ next to play
  | Winner Player    -- ^ game ended with a winner
  | Draw             -- ^ game ended with a draw
  deriving (Eq, Show)

-- | the game board
-- mapping coordinates to players
type Board = Map Coord Player

-- | game board size (constant)
boardSize :: Int
boardSize = 3

-- | the game position
data TicTacToe 
  = TicTacToe { state :: State
              , board :: Board
              } deriving Show


-- | switch to other player
switch :: Player -> Player
switch X = O
switch O = X

-- | initial game position
startGame :: Player -> TicTacToe
startGame player
  = TicTacToe { state = Playing player
              , board = Map.empty
              }

-- | check the winning condition for a board
winner :: Board -> Maybe Player
winner board = msum $ map checkWin (diag1 : diag2 : rows ++ cols)
  where row i = [Map.lookup (i,j) board | j<-[1..boardSize]]
        col j = [Map.lookup (i,j) board | i<-[1..boardSize]]
        diag1 = [Map.lookup (i,i) board | i<-[1..boardSize]]
        diag2 = [Map.lookup (i,boardSize+1-i) board | i<-[1..boardSize]]
        rows = map row [1..boardSize]
        cols = map col [1..boardSize]


-- | check the winning condition for a line
checkWin :: [Maybe Player] -> Maybe Player
checkWin (first@(Just p):rest)
  | all (==first) rest = first
checkWin _             = Nothing


-- | list of available coordinates in a board
freeCoordinates :: Board -> [Coord]
freeCoordinates board
  = [(i,j) |  i<-[1..boardSize], j<-[1..boardSize],
     (i,j) `Map.notMember` board]

-- | list possible moves from a position
availableMoves :: TicTacToe -> [Coord]
availableMoves position
  | Playing _ <- state position = freeCoordinates (board position)
  | otherwise                   = []

-- | play a valid move
makeMove :: Coord -> TicTacToe -> TicTacToe
makeMove coord position
  | Playing player <- state position = 
      let board' = Map.insert coord player (board position)
      in TicTacToe { state = nextState player board'
                   , board = board'
                   }
  | otherwise = error "makeMove: game has ended"

-- | helper function; advance to next state
nextState :: Player -> Board -> State
nextState player board
  | Just player' <- winner board = Winner player
  | null (freeCoordinates board) = Draw
  | otherwise                    = Playing (switch player)



