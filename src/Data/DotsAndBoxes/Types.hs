module Data.DotsAndBoxes.Types where

import Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.List as L
import Control.Arrow
import Data.Char

-- | Orientation of a move
data Orientation = Horizontal | Vertical deriving (Show, Eq, Ord)

-- | A move in the game.
--   The player is not specified because it can be inferred from the previous moves.
data Move = Move { _moveOrientation :: Orientation, _moveX :: Int, _moveY :: Int } deriving (Show, Eq, Ord)

-- | The player whose turn it is
data Player = Player1 | Player2 deriving (Show, Eq)

-- | The result of the game
data GameResult = NotFinished | Player1Win | Player2Win | Draw deriving (Show, Eq)

-- | Represents a 'winnable' box on the board
data Box = Box { _posX :: Int, _posY :: Int} deriving (Show, Ord, Eq)

-- | Board information at a point in the game
data BoardCurrentState =
  BoardCurrentState {
    _boardCurrentStateWidthInDots :: Int,
    _boardCurrentStateHeightInDots :: Int,
    _boardCurrentStateMoves :: Set.Set Move,
    _boardCurrentStateBoxes :: Map.Map Box Player, -- | Maps a box to the winner of that box
    _boardCurrentStatePlayer :: Player }
  deriving Eq

emptyBoardCurrentState :: Int -> Int -> BoardCurrentState
emptyBoardCurrentState w h = BoardCurrentState w h Set.empty Map.empty Player1

instance Show BoardCurrentState where
  show (BoardCurrentState w h ms bs _) =
    let isDot x y = mod x 2 == 0 && mod y 2 == 0 in
    let _lines = Map.fromList $ Prelude.map (lineCoordinate &&& lineChar) (Set.toList ms) in
    let _wins = Map.mapKeys boxCoordinate.
               Map.map playerDigit $ bs in
    concatMap (\y ->
        Prelude.map (\x ->
          if isDot x y
            then '.'
            else fromMaybe3 ' ' (marking x y) (Map.lookup (x, y) _lines) (Map.lookup (x, y) _wins)
            ) [1..displayWidth] ++ "\n")
          [1..displayHeight]
    where
      marking 1 y = if mod y 2 == 0 then Just (chr (ord 'a' + quot y 2 - 1)) else Nothing
      marking x 1 = if mod x 2 == 0 then Just (chr (ord 'a' + quot x 2 - 1)) else Nothing
      marking _ _ = Nothing
      displayWidth = w * 2
      displayHeight = h * 2
      playerDigit Player1 = '1'
      playerDigit Player2 = '2'
      boxCoordinate (Box x y) = (x * 2 + 1, y * 2 + 1)
      lineCoordinate (Move Horizontal x y) = (x * 2 + 1, y * 2)
      lineCoordinate (Move Vertical x y) = (x * 2, y * 2 + 1)
      lineChar (Move Horizontal _ _) = '-'
      lineChar (Move Vertical _ _) = '|'
      fromMaybe2 def m1 = fromMaybe (fromMaybe def m1)
      fromMaybe3 def m1 = fromMaybe2 (fromMaybe def m1)
