
module TAPF.AI (minimaxMove) where

import TAPF.TicTacToe
import TAPF.Minimax 

import           Data.List (maximumBy)
import           Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import           Data.Function (on)

instance Minimax TicTacToe where
  -- sucessors of a position: apply all available moves
  successors p = [makeMove m p | m <- availableMoves p]
  -- naive scoring function:
  -- if game ended in draw score 0
  -- if opponent has won score -infinity;
  -- otherwise compute diference between material
  valuation position
    = case state position of
        Draw      -> 0
        Winner _  -> -10000
        Playing player -> material player (board position) -
                          material (switch player) (board position)


-- | material value of a player in a board
-- sum of squared lengths of available lines for winning 
material :: Player -> Board -> Score
material player board = sum $ map score (diag1 : diag2 : rows ++ cols)
  where
    row i = catMaybes [Map.lookup (i,j) board | j<-[1..boardSize]]
    col j = catMaybes [Map.lookup (i,j) board | i<-[1..boardSize]]
    diag1 = catMaybes [Map.lookup (i,i) board | i<-[1..boardSize]]
    diag2 = catMaybes [Map.lookup (i,boardSize+1-i) board | i<-[1..boardSize]]
    rows = map row [1..boardSize]
    cols = map col [1..boardSize]
    score line = if all (==player) line then (length line)^2 else 0
               


-- | compute best move using the minimax value
minimaxMove :: Depth -> TicTacToe -> (Coord, Score)
minimaxMove depth p
  = maximumBy (compare `on` snd) moves
  where moves =  [ (m, - minimax depth p')  | m <- availableMoves p
                                            , let p' = makeMove m p ]
    
