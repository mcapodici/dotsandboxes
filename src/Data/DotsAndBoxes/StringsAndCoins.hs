{-# LANGUAGE ScopedTypeVariables #-}
module Data.DotsAndBoxes.StringsAndCoins where

import Data.DotsAndBoxes.Types
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Array
import Data.Tree
import Data.List
import Data.Tuple
import qualified Data.DotsAndBoxes.Functions as DABF
import qualified Data.Set as Set

type NSGraph = Gr () ()
data ChainInfo = ForwardConnections Int | Ground deriving (Show, Eq)
data Nimber = Nimber Int | Loony deriving (Show, Eq)
instance Ord Nimber where
  compare (Nimber x) (Nimber y) = compare x y
  compare Loony Loony = EQ
  compare Loony _ = GT
  compare _ Loony = LT

availableMoves :: BoardCurrentState -> [Move]
availableMoves (BoardCurrentState w h ms _ _) =
  Set.toList $ Set.difference
    (Set.fromList $ [Move Horizontal x y | x <- [1..w-1], y <- [1..h]] ++ [Move Vertical x y | x <- [1..w], y <- [1..h-1]])
    ms

convertToGraph :: BoardCurrentState -> Gr () ()
convertToGraph b@(BoardCurrentState w h ms bs p) =
  mkUGraph [0.. boxW * boxH] removedVertices      
  where
    boxW = w - 1
    boxH = h - 1
    removedVertices = concatMap 
      (\m -> case (DABF.affectedBoxes w h m) of 
        -- Todo non exhaustive, I think affectedBoxes should return (Box,Maybe Box)anyway
        [b] -> [(boxToNode b, 0)]
        b1 : b2 : _ -> [(boxToNode b1, boxToNode b2), (boxToNode b2, boxToNode b1)]
      ) 
      (availableMoves b)
  
    boxToNode :: Box -> Int
    boxToNode (Box x y) = (y - 1) * boxW + x 
 

-- | Returns all of the moves - where the bidirectional edgges which as a pair are reperesnted by one move
--   where the first vertex index is the greater. The only single directional edges in our graph will be to 0
--   so they are natrually included too
moves :: DynGraph gr => gr () () -> [Edge]
moves = filter (uncurry (>)) . edges 

makeMove :: DynGraph gr => Edge -> gr () () -> gr () ()
makeMove m = let delOneEdge = \e -> delLEdge (toLEdge e ()) in  (delOneEdge m) . (delOneEdge (swap m))

mex :: [Nimber] -> Nimber
mex ns = Nimber $ foldl' (\mex n -> if n == Nimber mex then mex + 1 else mex) 0 (sort ns)

nimber :: DynGraph gr => gr () () -> Nimber
nimber g= case () of
  _ | null (nodes g) -> Nimber 0
    | isLoony g -> Loony
    | otherwise -> mex (map (nimber . flip makeMove g) (moves g))

instatakesFor :: DynGraph gr => gr () ()  -> [Node]
instatakesFor g = filter (canInstatake g) (capturableCoins g)

capturableCoins :: DynGraph gr => gr () ()  -> [Node]
capturableCoins g = [n | n <- nodes g, length (suc g n) == 1]

isLoony :: DynGraph gr => gr () () -> Bool
isLoony g = any (isLoonyFor g) (capturableCoins g)

isLoonyFor :: DynGraph gr => gr () () -> Node -> Bool
isLoonyFor g n = case chainInfo g n of
    [ForwardConnections 1, ForwardConnections 1, ForwardConnections 0] -> False
    ForwardConnections 1 : ForwardConnections 1 : _  -> True
    otherwise -> False

canInstatake :: DynGraph gr => gr () () -> Node -> Bool
canInstatake g n = case chainInfo g n of
    [ForwardConnections 1, ForwardConnections 1, ForwardConnections 0] -> True
    ForwardConnections 1 : ForwardConnections 1 : _ -> False
    ForwardConnections 1 : _ -> True 
    otherwise -> False
          
chainInfo :: forall gr. DynGraph gr => gr () () -> Node -> [ChainInfo]
chainInfo graph initialNode = explore (match initialNode graph)
  where explore :: Decomp gr () () -> [ChainInfo]
        explore (Just (_, node, _, adj), newGraph) = 
          let value = if node == 0 then Ground else ForwardConnections (length adj) in
          let rest = if (length adj == 1) then explore ((match (snd (head adj)) newGraph)::Decomp gr () ())  else [] in
          value : rest


