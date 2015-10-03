module Data.DotsAndBoxes.Functions 
  (otherPlayer, 
  boxCount, 
  winner, 
  parseMove, 
  makeMove,
  affectedBoxes,  
  MoveError(..)
  ) where

import Data.DotsAndBoxes.Types
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import Data.Maybe
import Control.Arrow
import Data.Char
import Control.Monad.Except

data MoveError = MoveOutOfBounds | MoveAlreadyMade deriving (Show, Eq)
type MoveMonad = Either MoveError

wonBoxes :: BoardCurrentState -> [Player]
wonBoxes b = Map.elems (_boardCurrentStateBoxes b)

boxCount :: Player -> BoardCurrentState -> Int
boxCount p b = length (Prelude.filter (==p) (wonBoxes b))

winner :: BoardCurrentState -> GameResult
winner b@(BoardCurrentState w h ms _ _)
  | Set.size ms < (w-1)*h+w*(h-1) = NotFinished
  | p1Count > p2Count = Player1Win
  | p1Count < p2Count = Player2Win
  | otherwise = Draw
  where p1Count = boxCount Player1 b
        p2Count = boxCount Player2 b

-- | Parse a move like df- or ef1 where the first letter (upper) is the row, the second
-- | letter (lower) is the column and 1 is a vertical move down from the position and - is a Horizontal
-- | move right from the position
parseMove :: BoardCurrentState -> String -> Maybe Move
parseMove (BoardCurrentState w h ms _ _) [cy, cx, co] = do
  x <- let pos = ord cx - (ord 'a' - 1) in if pos <= 0 || pos > w then Nothing else Just pos
  y <- let pos = ord cy - (ord 'a' - 1) in if pos <= 0 || pos > h then Nothing else Just pos
  o <- case co of
    '1' -> Just Vertical
    '-' -> Just Horizontal
    otherwise -> Nothing
  if x == w && o == Horizontal || y == h && o == Vertical || Set.member (Move o x y) ms
      then Nothing
      else return (Move o x y)
parseMove _ _ = Nothing

makeMove :: Move -> BoardCurrentState -> MoveMonad BoardCurrentState
makeMove m@(Move o x y) (BoardCurrentState w h ms bs p)
  | x < 1 ||
      y < 1 || x >= w && o == Horizontal || y >= h && o == Vertical
    = throwError MoveOutOfBounds
  | Set.member (Move o x y) ms = throwError MoveAlreadyMade
  | otherwise =
    return $
      BoardCurrentState w h movesAfter bsAfter
        ((if L.null boxesWon then otherPlayer else id) p)
  where movesAfter = Set.insert m ms
        bsAfter = L.foldl' (\ bs2 bx -> Map.insert bx p bs2) bs boxesWon

        boxesWon :: [Box]
        boxesWon = L.filter boxWon (affectedBoxes w h m)

        boxWon :: Box -> Bool
        boxWon = all (`Set.member` movesAfter) . associatedMoves

        associatedMoves :: Box -> [Move]
        associatedMoves (Box x2 y2)
          = [Move Horizontal x2 y2, Move Vertical x2 y2,
             Move Horizontal x2 (y2 + 1), Move Vertical (x2 + 1) y2]
            
affectedBoxes :: Int -> Int -> Move -> [Box]
affectedBoxes w h (Move o x y) = 
  case o of
    Horizontal -> case () of
      _ | y == 1 -> [Box x y]
        | y == h -> [Box x (y-1)]
        | otherwise -> [Box x y, Box x (y-1)]
    Vertical -> case () of
      _ | x == 1 -> [Box x y]
        | x == w -> [Box (x-1) y]
        | otherwise -> [Box (x-1) y, Box x y]
{--
first ++ second 
  where
    first = case () of
          _ | o == Horizontal && y > 1 -> [Box x (y - 1)]
            | o == Vertical && x > 1 -> [Box (x - 1) y]
            | otherwise -> []

    second =
  = Box x y :
      case () of
          _ | o == Horizontal && y > 1 -> [Box x (y - 1)]
            | o == Vertical && x > 1 -> [Box (x - 1) y]
            | otherwise -> []
--}
otherPlayer :: Player -> Player
otherPlayer Player1 = Player2
otherPlayer Player2 = Player1