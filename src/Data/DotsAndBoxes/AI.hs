module Data.DotsAndBoxes.AI where

import Data.Either
import qualified Data.Set as Set
import Data.DotsAndBoxes.Types
import Data.DotsAndBoxes.Functions
import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout
import Data.Graph

type PlayerAI = BoardCurrentState -> Maybe Move
newtype BoardCurrentStateAndPlayerView = BCSPV { unBCSPV :: (Player, BoardCurrentState) }

availableMoves :: BoardCurrentState -> [Move]
availableMoves (BoardCurrentState w h ms _ _) =
  Set.toList $ Set.difference
    (Set.fromList $ [Move Horizontal x y | x <- [1..w-1], y <- [1..h]] ++ [Move Vertical x y | x <- [1..w], y <- [1..h-1]])
    ms

dumbAI :: PlayerAI
dumbAI b = case availableMoves b of
  [] -> Nothing
  (x:_) -> Just x
  
instance Game_tree BoardCurrentStateAndPlayerView where
  is_terminal = (/= NotFinished) . winner . snd . unBCSPV
  node_value bap = let b = (snd . unBCSPV) bap in (boxCount Player2 b) - (boxCount Player1 b)
  children (BCSPV (p, b)) = if (p == _boardCurrentStatePlayer b) 
    then map (BCSPV . (,) (otherPlayer p)) (rights (map (\m -> makeMove m b) (availableMoves b))) 
    else [BCSPV (otherPlayer p,b)] -- A player getting 2 consecutive turns is modelled as the other player missing their turn, 
                 -- in which case the only 'move' is to do nothing i.e. return the same board

wrapWithPlayer :: BoardCurrentState -> BoardCurrentStateAndPlayerView
wrapWithPlayer b = BCSPV (_boardCurrentStatePlayer b, b)
  
negascoutAI :: PlayerAI
negascoutAI b = case fst (negascout (wrapWithPlayer b) 2) of
  [] -> Nothing
  (_:BCSPV(_, newB):_) -> Just (Set.elemAt 0 (Set.difference (_boardCurrentStateMoves newB) (_boardCurrentStateMoves b)))

-- TODO: USE dff to convert graph, then can analyse each 'potential chain' to see if it is a chain, if it is, and it is takeable then decide whether to double cross.  
  
convertToGraph :: BoardCurrentState -> Graph
convertToGraph b@(BoardCurrentState w h ms bs p) =
  buildG (0, boxW * boxH) removedVertices      
  where
    boxW = w - 1
    boxH = h - 1
    removedVertices = concatMap 
      (\m -> case (affectedBoxes w h m) of 
        -- Todo non exhaustive, I think affectedBoxes should return (Box,Maybe Box)anyway
        [b] -> [(boxToNode b, 0)]
        b1 : b2 : _ -> [(boxToNode b1, boxToNode b2), (boxToNode b2, boxToNode b1)]
      ) 
      (availableMoves b)
  
    boxToNode :: Box -> Int
    boxToNode (Box x y) = (y - 1) * boxW + x 
    
    