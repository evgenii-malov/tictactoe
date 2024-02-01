module Main where

import Test.HUnit
import TicTacToe (CellData (..), Player (..), hasWinPattern)

-- Create a test case
testWinV1 :: Test
testWinV1 = TestCase (assertEqual "Test Vertical Line win pattern with x = 0 for O player" True (hasWinPattern [O, X, X, O, X, X, O, X, X] PO))

testWinV2 :: Test
testWinV2 = TestCase (assertEqual "Test Vertical Line win pattern with x = 1 for O player" True (hasWinPattern [X, O, X, O, O, X, O, O, X] PO))

-- Sorry I have no time to write them all :)

-- Create a test suite
tests :: Test
tests =
  TestList
    [ TestLabel "Test Vert Line with x = 0" testWinV1,
      TestLabel "Test Vert Line with x = 1" testWinV2
    ]

-- Run the test suite
-- main :: IO Counts
main = do
  runTestTT tests
