module Day2
  ( solveDay2
  ) where

import Data.List

solveDay2 :: String
solveDay2 = show (trySolve inputDay2 (12, 2))

trySolve :: [Int] -> (Int, Int) -> Int
trySolve tape (noun, verb) = head $ computeTape $ restore noun verb tape
  where
    restore noun verb tape = take 1 tape ++ [noun, verb] ++ drop 3 tape

findSolution :: [Int] -> Maybe (Int, Int)
findSolution tape =
  case ix of
    Just i  -> Just (candidates !! i)
    Nothing -> Nothing
  where
    ix = Data.List.findIndex (isSolved tape) candidates
    isSolved tape (noun, verb) = trySolve tape (noun, verb) == 19690720
    candidates = [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]

computeTape = next' 0

next' :: Int -> [Int] -> [Int]
next' pos tape
  | pos >= length tape || tape !! pos == 99 = tape
  | otherwise = next' (pos + 4) (next pos tape)

next :: Int -> [Int] -> [Int]
next pos tape
    -- end
  | opcode == 99 = tape
    --adds
  | opcode == 1 --trace ("1:" ++ show [op1, op2, resPos])
   = take resPos tape ++ [op1 + op2] ++ drop (resPos + 1) tape
    --multiplies
  | opcode == 2 = take resPos tape ++ [op1 * op2] ++ drop (resPos + 1) tape
  | otherwise = error ("unknown opcode!" ++ show opcode ++ " at " ++ show pos ++ " state:" ++ show tape)
  where
    opcode = tape !! pos
    op1 = tape !! (tape !! (pos + 1))
    op2 = tape !! (tape !! (pos + 2))
    resPos = tape !! (pos + 3)
    tapeLen = length tape

inputDay2 :: [Int]
inputDay2 =
  [ 1
  , 0
  , 0
  , 3
  , 1
  , 1
  , 2
  , 3
  , 1
  , 3
  , 4
  , 3
  , 1
  , 5
  , 0
  , 3
  , 2
  , 6
  , 1
  , 19
  , 1
  , 19
  , 5
  , 23
  , 2
  , 9
  , 23
  , 27
  , 1
  , 5
  , 27
  , 31
  , 1
  , 5
  , 31
  , 35
  , 1
  , 35
  , 13
  , 39
  , 1
  , 39
  , 9
  , 43
  , 1
  , 5
  , 43
  , 47
  , 1
  , 47
  , 6
  , 51
  , 1
  , 51
  , 13
  , 55
  , 1
  , 55
  , 9
  , 59
  , 1
  , 59
  , 13
  , 63
  , 2
  , 63
  , 13
  , 67
  , 1
  , 67
  , 10
  , 71
  , 1
  , 71
  , 6
  , 75
  , 2
  , 10
  , 75
  , 79
  , 2
  , 10
  , 79
  , 83
  , 1
  , 5
  , 83
  , 87
  , 2
  , 6
  , 87
  , 91
  , 1
  , 91
  , 6
  , 95
  , 1
  , 95
  , 13
  , 99
  , 2
  , 99
  , 13
  , 103
  , 1
  , 103
  , 9
  , 107
  , 1
  , 10
  , 107
  , 111
  , 2
  , 111
  , 13
  , 115
  , 1
  , 10
  , 115
  , 119
  , 1
  , 10
  , 119
  , 123
  , 2
  , 13
  , 123
  , 127
  , 2
  , 6
  , 127
  , 131
  , 1
  , 13
  , 131
  , 135
  , 1
  , 135
  , 2
  , 139
  , 1
  , 139
  , 6
  , 0
  , 99
  , 2
  , 0
  , 14
  , 0
  ]
