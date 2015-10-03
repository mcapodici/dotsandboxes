module Main where

import Data.DotsAndBoxes.Types
import Data.DotsAndBoxes.Functions
import Data.DotsAndBoxes.AI
import Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import Data.Maybe

main :: IO ()
main = do
  putStrLn "Welcome to dots and boxes."
  putStrLn "To play a move enter the row letter, the column letter, then the orientation (either dash: - or one: 1). E.g. aa-"
  gameLoop (emptyBoardCurrentState 4 4)

gameLoop :: BoardCurrentState -> IO ()
gameLoop b = do
  getMoveResult <- getMove b
  case getMoveResult of
    Left Quit -> putStrLn "quit game. bye!"
    Right newB -> case winner newB of
      NotFinished -> gameLoop newB
      otherwise -> gameOver newB

gameOver :: BoardCurrentState -> IO ()
gameOver b = do
  putStrLn "Game Over! Final board:"
  print b
  putStrLn $ "Player1 Boxes: " ++ show (boxCount Player1 b)
  putStrLn $ "Player2 Boxes: " ++ show (boxCount Player2 b)
  putStrLn $ "Winner: " ++ case winner b of
    Player1Win -> "Player 1"
    Player2Win -> "Player 2"
    Draw -> "Draw"

getMove :: BoardCurrentState -> IO (Either Quit BoardCurrentState)
getMove b@(BoardCurrentState _ _ _ _ Player1) = getHumanMove b
getMove b@(BoardCurrentState _ _ _ _ Player2) = getAIMove b
    
getAIMove :: BoardCurrentState -> IO (Either Quit BoardCurrentState)
getAIMove b = do
  let maybeMove = negascoutAI b
  let aiException = putStrLn "Problem with AI. Quitting" >> return (Left Quit)
  case maybeMove of 
    Nothing -> aiException
    Just move -> case makeMove move b of
      Left error -> aiException
      Right newB -> return (Right newB)
        
getHumanMove :: BoardCurrentState -> IO (Either Quit BoardCurrentState)
getHumanMove b = do
  print b
  putStrLn $ "It is the turn of " ++ show (_boardCurrentStatePlayer b)
  putStrLn "Please enter your move below or q to quit"
  getHumanMoveLoop b

getHumanMoveLoop :: BoardCurrentState -> IO (Either Quit BoardCurrentState)
getHumanMoveLoop b = do
  moveString <- getLine
  if moveString == "q" then return (Left Quit) else
    let maybeMove = parseMove b moveString in
    case maybeMove of
      Nothing -> putStrLn "Move is not valid, please enter another move." >> getHumanMoveLoop b
      Just move -> case makeMove move b of
        Left error -> putStrLn "Error Ocurred" >> getHumanMoveLoop b
        Right newB -> return (Right newB)

data Quit = Quit
