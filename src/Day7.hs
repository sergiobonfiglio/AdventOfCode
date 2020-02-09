module Day7
  ( solve
  , solvePart2
  ) where

import           Data.List
import           Debug.Trace

data Progress
  = NotStarted
  | Processing
  | WaitInput
  | Halted
  deriving (Eq, Show)

type MachineState = (Int, [Int], [Int], [Int], Progress)

--TODO: try with record syntax
--  MachineState
--    { position :: Int
--    , tape ::     [Int]
--    , inp ::      [Int]
--    , out ::      [Int]
--    , progress :: Progress
--    }
solve = show $ snd $ solveFor input

solvePart2 = show $ snd $ solveForPart2 input

solveFor input = maximumBy (\(c1, v1) (_, v2) -> compare v1 v2) results
  where
    results = map (\x -> (x, trySequence input x)) (permutations [0 .. 4])

solveForPart2 input = maximumBy (\(c1, v1) (_, v2) -> compare v1 v2) results
  where
    results = map (\x -> (x, trySequenceLoop input x)) (permutations [5 .. 9])

computeOutput tape input = last outputs
  where
    (_, _, _, outputs, _) = computeProgram (0, tape, input, [], NotStarted)

trySequence :: [Int] -> [Int] -> Int
trySequence input = foldl (\acc x -> computeOutput input [x, acc]) 0

trySequenceLoop :: [Int] -> [Int] -> Int
trySequenceLoop input settings = loopUntilHalt 0 $ initAmps input settings

initAmps :: [Int] -> [Int] -> [MachineState]
initAmps tape [a, b, c, d, e] =
  [ (0, tape, [a], [], NotStarted)
  , (0, tape, [b], [], NotStarted)
  , (0, tape, [c], [], NotStarted)
  , (0, tape, [d], [], NotStarted)
  , (0, tape, [e], [], NotStarted)
  ]

loopUntilHalt :: Int -> [MachineState] -> Int
loopUntilHalt x allAmps
  | getProgress lastAmp == Halted = getOut lastAmp
  | otherwise =
    let lastLoop = loop x allAmps
        (_, _, _, lastOut, _) = last lastLoop
     in loopUntilHalt (last lastOut) lastLoop
  where
    firstAmp = head allAmps
    lastAmp = last allAmps

loop :: Int -> [MachineState] -> [MachineState]
loop x =
  foldl'
    (\allAmps curMs ->
       let lastOut =
             if null allAmps
               then x
               else getOut (last allAmps)
           ms@(_, _, _, output, progress) = computeProgram (applyInput curMs lastOut)
        in allAmps ++ [ms])
    []
  where
    applyInput :: MachineState -> Int -> MachineState
    applyInput (pos, tape, input, out, progress) x = (pos, tape, input ++ [x], out, progress)
    
getOut :: MachineState -> Int
getOut (pos, tape, input, out, progress) = last out
getProgress :: MachineState -> Progress
getProgress (pos, tape, input, out, progress) = progress

computeProgram :: MachineState -> MachineState
computeProgram ms@(pos, tape, input, out, progress)
  | progress == Halted || (progress == WaitInput && null input) = ms
  | otherwise = computeProgram newMs
  where
    newMs = next ms

next :: MachineState -> MachineState
next (posIn, tapeIn, input, outputs, progress)
    -- end
  | opcode == 99 = (posIn, tapeIn, input, outputs, Halted)
    --adds
  | opcode == 1 = (posIn + 4, updateTape param3 (param1 + param2), input, outputs, Processing)
    --multiplies
  | opcode == 2 = (posIn + 4, updateTape param3 (param1 * param2), input, outputs, Processing)
  --inputs
  | opcode == 3 =
    if not (null input)
      then (posIn + 2, updateTape val1 (head input), tail input, outputs, Processing)
      else (posIn, tapeIn, input, outputs, WaitInput)
  --outputs
  | opcode == 4 = (posIn + 2, tapeIn, input, outputs ++ [param1], Processing)
  --jump-if-true
  | opcode == 5 =
    ( if param1 /= 0
        then param2
        else posIn + 3
    , tapeIn
    , input
    , outputs
    , Processing)
  --jump-if-false
  | opcode == 6 =
    ( if param1 == 0
        then param2
        else posIn + 3
    , tapeIn
    , input
    , outputs
    , Processing)
  --less than
  | opcode == 7 =
    ( posIn + 4
    , updateTape
        param3
        (if param1 < param2
           then 1
           else 0)
    , input
    , outputs
    , Processing)
  --equals
  | opcode == 8 =
    ( posIn + 4
    , updateTape
        param3
        (if param1 == param2
           then 1
           else 0)
    , input
    , outputs
    , Processing)
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

test1p2 :: [Int]
test1p2 =
  [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5]

test2p2 :: [Int]
test2p2 =
  [ 3
  , 52
  , 1001
  , 52
  , -5
  , 52
  , 3
  , 53
  , 1
  , 52
  , 56
  , 54
  , 1007
  , 54
  , 5
  , 55
  , 1005
  , 55
  , 26
  , 1001
  , 54
  , -5
  , 54
  , 1105
  , 1
  , 12
  , 1
  , 53
  , 54
  , 53
  , 1008
  , 54
  , 0
  , 55
  , 1001
  , 55
  , 1
  , 55
  , 2
  , 53
  , 55
  , 53
  , 4
  , 53
  , 1001
  , 56
  , -1
  , 56
  , 1005
  , 56
  , 6
  , 99
  , 0
  , 0
  , 0
  , 0
  , 10
  ]

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
