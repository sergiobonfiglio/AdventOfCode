module Day7
  ( solve
  ) where

import           Data.List

type MachineState = (Int, [Int], [Int], [Int])

trySequence :: [Int] -> [Int] -> Int
trySequence input = foldl (\acc x -> output input [x, acc]) 0

solve = solveFor input

solveFor input = maximumBy (\(c1, v1) (_, v2) -> compare v1 v2) results
  where
    results = map (\x -> (x, trySequence input x)) (permutations [0 .. 4])

output tape input = last outputs
  where
    (_, _, _, outputs) = next' (0, tape, input, [])

next' :: MachineState -> MachineState
next' (pos, tape, input, out)
  | pos < 0 || pos >= length tape = (pos, tape, input, out)
  | otherwise = next' (nextPos, nextTape, nextInteger, nextOut)
  where
    (nextPos, nextTape, nextInteger, nextOut) = next (pos, tape, input, out)

next :: MachineState -> MachineState
next (posIn, tapeIn, input, outputs)
    -- end
  | opcode == 99 = (-1, tapeIn, input, outputs)
    --adds
  | opcode == 1 = (posIn + 4, updateTape param3 (param1 + param2), input, outputs)
    --multiplies
  | opcode == 2 = (posIn + 4, updateTape param3 (param1 * param2), input, outputs)
  --inputs
  | opcode == 3 = (posIn + 2, updateTape val1 (head input), tail input, outputs)
  --outputs
  | opcode == 4 = (posIn + 2, tapeIn, input, outputs ++ [param1])
  --jump-if-true
  | opcode == 5 =
    ( if param1 /= 0
        then param2
        else posIn + 3
    , tapeIn
    , input
    , outputs)
  --jump-if-false
  | opcode == 6 =
    ( if param1 == 0
        then param2
        else posIn + 3
    , tapeIn
    , input
    , outputs)
  --less than
  | opcode == 7 =
    ( posIn + 4
    , updateTape
        param3
        (if param1 < param2
           then 1
           else 0)
    , input
    , outputs)
  --equals
  | opcode == 8 =
    ( posIn + 4
    , updateTape
        param3
        (if param1 == param2
           then 1
           else 0)
    , input
    , outputs)
  | otherwise = error ("unknown opcode!" ++ show opcode ++ " at " ++ show posIn)
  where
    updateTape i x = take i tapeIn ++ [x] ++ drop (i + 1) tapeIn
    val0 = tapeIn !! posIn
    val1 = tapeIn !! (posIn + 1)
    opcode = val0 `mod` 100
    p1Mode = (val0 `div` 100) `mod` 10
    p2Mode = (val0 `div` 1000) `mod` 10
    p3Mode = (val0 `div` 10000) `mod` 10
    param1 = paramVal (posIn + 1) p1Mode
    param2 = paramVal (posIn + 2) p2Mode
    param3 = tapeIn !! (posIn + 3) --only look position for output-- paramVal (posIn + 3) p3Mode
    paramVal pos mode =
      if mode == 0
        then tapeIn !! fromIntegral (tapeIn !! pos) --position mode
        else tapeIn !! pos --immediate mode

test1 :: [Int]
test1 = [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]

test2 :: [Int]
test2 = [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0]

test3 :: [Int]
test3 =
  [ 3
  , 31
  , 3
  , 32
  , 1002
  , 32
  , 10
  , 32
  , 1001
  , 31
  , -2
  , 31
  , 1007
  , 31
  , 0
  , 33
  , 1002
  , 33
  , 7
  , 33
  , 1
  , 33
  , 31
  , 31
  , 1
  , 32
  , 31
  , 31
  , 4
  , 31
  , 99
  , 0
  , 0
  , 0
  ]

input :: [Int]
input =
  [ 3
  , 8
  , 1001
  , 8
  , 10
  , 8
  , 105
  , 1
  , 0
  , 0
  , 21
  , 30
  , 55
  , 80
  , 101
  , 118
  , 199
  , 280
  , 361
  , 442
  , 99999
  , 3
  , 9
  , 101
  , 4
  , 9
  , 9
  , 4
  , 9
  , 99
  , 3
  , 9
  , 101
  , 4
  , 9
  , 9
  , 1002
  , 9
  , 4
  , 9
  , 101
  , 4
  , 9
  , 9
  , 1002
  , 9
  , 5
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 99
  , 3
  , 9
  , 101
  , 5
  , 9
  , 9
  , 1002
  , 9
  , 2
  , 9
  , 101
  , 3
  , 9
  , 9
  , 102
  , 4
  , 9
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 99
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 101
  , 5
  , 9
  , 9
  , 102
  , 3
  , 9
  , 9
  , 101
  , 3
  , 9
  , 9
  , 4
  , 9
  , 99
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 102
  , 4
  , 9
  , 9
  , 1001
  , 9
  , 3
  , 9
  , 4
  , 9
  , 99
  , 3
  , 9
  , 1001
  , 9
  , 1
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1002
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1002
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1002
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 101
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 101
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 101
  , 2
  , 9
  , 9
  , 4
  , 9
  , 99
  , 3
  , 9
  , 101
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 1
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1002
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 1
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 99
  , 3
  , 9
  , 1001
  , 9
  , 1
  , 9
  , 4
  , 9
  , 3
  , 9
  , 101
  , 1
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1002
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 99
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 101
  , 1
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 101
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 101
  , 1
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 101
  , 2
  , 9
  , 9
  , 4
  , 9
  , 99
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 101
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 101
  , 1
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1002
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 101
  , 1
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 1001
  , 9
  , 2
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 3
  , 9
  , 102
  , 2
  , 9
  , 9
  , 4
  , 9
  , 99
  ]
