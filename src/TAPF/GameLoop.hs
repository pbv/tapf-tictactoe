{-
  Game loop to play Tic-Tac-Toe with computer or human players
-}
module TAPF.GameLoop (play0, play1, play2) where

import           TAPF.TicTacToe
import           TAPF.AI
import           TAPF.Minimax
import qualified Data.Map.Strict as Map


-- | a human or AI bot
type Actor = Player -> Board -> IO Coord

-- | top-level game loop; player against a human, computer,
-- or watch the AI play against itself
play0, play1, play2 :: IO ()
play0 = play (computerPlay 5) (computerPlay 3)
play1 = play humanPlay (computerPlay 4)
play2 = play humanPlay humanPlay

play :: Actor -> Actor -> IO ()
play actor1 actor2 = do
  endgame <- playLoop actor1 actor2 (startGame X)
  case state endgame of
    Winner player -> putStrLn ("Player " ++ show player ++ " won")
    Draw          -> putStrLn "Draw"
  
-- | play loop until end of game
playLoop :: Actor -> Actor -> TicTacToe -> IO TicTacToe
playLoop actor1 actor2 position
  | Playing player <- state position = do
      printBoard position
      m <- actor1 player (board position)
      let position' = makeMove m position
      playLoop actor2 actor1 position'
  | otherwise = do
      printBoard position
      return position


-- | ask next move from user
humanPlay :: Player -> Board -> IO Coord
humanPlay player board = loop
  where 
    coords = freeCoordinates board
    loop = do
      putStrLn ("Player " ++ show player ++ "? ")
      txt <- getLine
      case reads txt of
        [(pos, "")] -> if pos `elem` coords
                       then return pos
                       else do msg; loop
        _ -> do msg; loop 
    msg = do
      putStr "invalid move; available moves are: "
      print coords


-- helper functions to print a board
printBoard :: TicTacToe -> IO ()
printBoard = putStrLn . showBoard . board

showBoard :: Board -> String
showBoard b
  = unlines [[charAt i j | j<-[1..boardSize]] | i<-[1..boardSize]]
  where
    charAt i j = case Map.lookup (i,j) b of
      Just p -> head (show p)
      Nothing -> '.'


-- | AI player
computerPlay :: Depth -> Player -> Board -> IO Coord
computerPlay depth player board =
  let (m,score) = minimaxMove depth (TicTacToe (Playing player) board)
  in do
    putStrLn ("Computer " ++ show player ++
              " depth "++ show depth ++ " plays " ++ show m)
    putStrLn ("Score "++ show score)
    return m
