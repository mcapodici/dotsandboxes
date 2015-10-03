import Data.DotsAndBoxes.Test.General
import Control.Monad
import System.Exit
import Test.HUnit

main :: IO ()
main = do
--    propSuccess <- runPropTests
--    unless propSuccess exitFailure
    hunitResult <- runTestTT testGroup
    unless (errors hunitResult == 0 && failures hunitResult == 0) exitFailure
