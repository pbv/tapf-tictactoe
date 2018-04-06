{-
  Game trees and simple Minimax algorithm (no alpha-beta prunning)
  Pedro Vasconcelos
  Based on chap. 9 of "Introduction to Functional Programming",
  by Richard Bird and Philip Wadler, 1988
-}
module TAPF.Minimax where


type Score = Int -- ^ position value score

-- | a type class for game position unfolding and valuation
class Minimax p where
  -- next game positions; empty if the game has ended
  successors :: p -> [p]
  -- valuation for a position
  -- from the point-of-view of the *active* player
  valuation :: p -> Score


-- | "Rose" trees, i.e. trees with variable number of children
data Tree a
  = Node a [Tree a]
  deriving (Eq, Show)

root :: Tree a -> a
root (Node x _) = x

children :: Tree a -> [Tree a]
children (Node _ ts) = ts

instance Functor Tree where
  fmap f (Node x ts) = Node (f x) (map (fmap f) ts)


type Depth = Int  -- tree depth limit (# levels)

-- | prune a tree to a given depth
prune :: Depth -> Tree a -> Tree a
prune n (Node p ts)
  | n>0       = Node p (map (prune (n-1)) ts)
  | otherwise = Node p []


-- | Unfold a game tree from a given position Because of lazy
-- evaluation, the tree will be computed "on demand"; this is useful
-- for games with large even infinite trees
gameTree :: Minimax p => p -> Tree p
gameTree p = Node p (map gameTree (successors p))


-- | compute the approximate minimax value up-to a given depth
minimax :: Minimax p => Depth -> p -> Score
minimax depth = minimax' . fmap valuation . prune depth . gameTree 

-- | auxiliary function; minimax of a finite tree decorated with
-- score values
minimax' :: Tree Score -> Score
minimax' (Node v []) = v
minimax' (Node _ ts) = negate $ minimum (map minimax' ts)
