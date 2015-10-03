module Data.DotsAndBoxes.Test.General where

import Control.Monad
import Test.HUnit
import Data.DotsAndBoxes.Types
import Data.DotsAndBoxes.Functions
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as L

exampleBoard = BoardCurrentState 3 3 (Set.fromList [Move Horizontal 1 1]) Map.empty Player2
almostCompleteGame = playMoves 3 3 [
                       Move Horizontal 1 1,
                       Move Horizontal 2 2,
                       Move Horizontal 1 3,
                       Move Horizontal 2 1,
                       Move Horizontal 1 2,
                       Move Horizontal 2 3,
                       Move Vertical 1 1,
                       Move Vertical 2 2,
                       Move Vertical 3 1,
                       Move Vertical 1 2,
                       Move Vertical 2 1]

completeGameP1Win = playMoves 3 3 [
                       Move Horizontal 1 1,
                       Move Horizontal 2 1,
                       Move Vertical 3 1,
                       Move Vertical 3 2,
                       Move Horizontal 2 3,
                       Move Horizontal 1 3,
                       Move Vertical 1 2,
                       Move Vertical 1 1,
                       Move Vertical 2 1,
                       Move Vertical 2 2,
                       Move Horizontal 1 2,
                       Move Horizontal 2 2]  

completeGameP2Win = playMoves 3 3 [
                       Move Horizontal 1 1,
                       Move Horizontal 2 2,
                       Move Horizontal 1 3,
                       Move Horizontal 2 1,
                       Move Horizontal 1 2,
                       Move Horizontal 2 3,
                       Move Vertical 1 1,
                       Move Vertical 2 2,
                       Move Vertical 3 1,
                       Move Vertical 1 2,
                       Move Vertical 2 1,
                       Move Vertical 3 2] 
               
testParseMove :: Test
testParseMove = TestCase $ do   
    forM_ [("aa-",Move Horizontal 1 1),
           ("aa1",Move Vertical 1 1),
           ("cb-",Move Horizontal 2 3),
           ("bc1",Move Vertical 3 2)] (\(good, expected) ->
      assertEqual ("Valid move " ++ good)
        (Just expected)
        (parseMove (emptyBoardCurrentState 3 3) good))
      
    forM_ ["aa", "aa2", "AA1", ""] (\bad ->
      assertEqual ("Invalid move (due to parsing) " ++ bad)
        Nothing
        (parseMove (emptyBoardCurrentState 3 3) bad))
        
    forM_ ["ca1", "ac-"] (\bad ->
      assertEqual ("Invalid move (due to out of bounds) " ++ bad)
        Nothing
        (parseMove (emptyBoardCurrentState 3 3) bad))
               
testBoxCount :: Test
testBoxCount = TestCase $ do   
    assertEqual "Empty game"
      (0, 0)
      (let t = flip boxCount (emptyBoardCurrentState 3 3) in (t Player1, t Player2))
      
    assertEqual "One move game"
      (0, 0)
      (let t = flip boxCount exampleBoard in (t Player1, t Player2))
          
    assertEqual "Almost Complete Game"
      (0, 3)
      (let t = flip boxCount almostCompleteGame in (t Player1, t Player2))
                       
    assertEqual "Complete Game P1 WIN"
      (4, 0)
      (let t = flip boxCount completeGameP1Win in (t Player1, t Player2))
                       
    assertEqual "Complete Game P2 WIN"
      (0, 4)
      (let t = flip boxCount completeGameP2Win in (t Player1, t Player2))
      
testWinner :: Test
testWinner = TestCase $ do   
    assertEqual "Empty game is incomplete"
      NotFinished
      (winner (emptyBoardCurrentState 3 3))
      
    assertEqual "One move game is incomplete"
      NotFinished
      (winner exampleBoard)
          
    assertEqual "Almost Complete Game"
      NotFinished
      (winner almostCompleteGame)
                       
    assertEqual "Complete Game P1 WIN"
      Player1Win
      (winner completeGameP1Win)
                       
    assertEqual "Complete Game P2 WIN"
      Player2Win
      (winner completeGameP2Win)
    
testMakeMove :: Test
testMakeMove = TestCase $ do
    assertEqual "Invalid move due to move already taken"
      (Left MoveAlreadyMade)
      (makeMove (Move Horizontal 1 1) exampleBoard)

    assertEqual "Invalid move due to out of range Horizontally (Right Edge)"
      (Left MoveOutOfBounds)
      (makeMove (Move Horizontal 3 1) exampleBoard)

    assertEqual "Invalid move due to out of range Horizontally (Beyond Right Edge)"
      (Left MoveOutOfBounds)
      (makeMove (Move Horizontal 4 1) exampleBoard)

    assertEqual "Invalid move due to out of range Horizontally (Beyond Left Edge)"
      (Left MoveOutOfBounds)
      (makeMove (Move Horizontal 0 1) exampleBoard)

    assertEqual "Invalid move due to out of range Vertically (Bottom Edge)"
      (Left MoveOutOfBounds)
      (makeMove (Move Vertical 1 3) exampleBoard)

    assertEqual "Invalid move due to out of range Vertically (Beyond Bottom Edge)"
      (Left MoveOutOfBounds)
      (makeMove (Move Vertical 1 4) exampleBoard)

    assertEqual "Invalid move due to out of range Vertically (Beyond Top Edge)"
      (Left MoveOutOfBounds)
      (makeMove (Move Vertical 1 0) exampleBoard)

    assertEqual "Case valid move on empty board"
      (Right (BoardCurrentState 3 3 (Set.fromList [Move Vertical 1 1]) Map.empty Player2))
      (makeMove (Move Vertical 1 1) (emptyBoardCurrentState 3 3))

    assertEqual "Case valid move on occupied board"
      (Right (BoardCurrentState 3 3 (Set.fromList [Move Horizontal 1 1, Move Vertical 1 1]) Map.empty Player1))
      (makeMove (Move Vertical 1 1) exampleBoard)

    assertEqual "Various moves with wins"
      (BoardCurrentState
                10
                3
                (Set.fromList [
                       Move Horizontal 1 1,
                       Move Horizontal 1 2,
                       Move Vertical 1 1,
                       Move Vertical 2 1,
                       Move Horizontal 2 1,
                       Move Horizontal 2 2,
                       Move Horizontal 7 2,
                       Move Vertical 3 1])
                (Map.fromList [
                      (Box 1 1, Player2),
                      (Box 2 1, Player1)])
                Player1)
      (playMoves 10 3 [Move Horizontal 1 1,
                       Move Horizontal 1 2,
                       Move Vertical 1 1,
                       Move Vertical 2 1,
                       Move Horizontal 2 1,
                       Move Horizontal 2 2,
                       Move Horizontal 7 2,
                       Move Vertical 3 1])

testShowBoard :: Test
testShowBoard = TestCase $ do
    assertEqual  "Case 2 by 2 with no moves"
                 " a b\n\
                 \a. .\n\
                 \    \n\
                 \b. .\n" $
                 show $ emptyBoardCurrentState 2 2
    assertEqual  "Case 10 by 3 with some moves"
                 " a b c d e f g h i j\n\
                 \a.-.-. . . . . . . .\n\
                 \ |2|1|              \n\
                 \b.-.-. . . . .-. . .\n\
                 \                    \n\
                 \c. . . . . . . . . .\n" $
                 show $ playMoves 10 3 [Move Horizontal 1 1,
                                    Move Horizontal 1 2,
                                    Move Vertical 1 1,
                                    Move Vertical 2 1,
                                    Move Horizontal 2 1,
                                    Move Horizontal 2 2,
                                    Move Horizontal 7 2,
                                    Move Vertical 3 1]

playMoves :: Int -> Int -> [Move] -> BoardCurrentState
playMoves w h = foldl (\s m -> either (error . show) id (makeMove m s)) (emptyBoardCurrentState w h)

testGroup :: Test
testGroup =
    TestList
        [
            TestLabel "show instance for BoardCurrentState" testShowBoard,
            TestLabel "makeMove" testMakeMove,
            TestLabel "winner" testWinner,
            TestLabel "boxCount" testBoxCount,
            TestLabel "parseMove" testParseMove
        ]
