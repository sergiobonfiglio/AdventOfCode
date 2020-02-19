module Main where

import           Day1
import           Day10
import           Day2
import           Day3
import           Day4
import           Day5
import           Day6
import           Day7
import           Day8
import           Day9

main :: IO ()
main = do
  putStrLn ("Day 1:" ++ solveDay1)
  putStrLn $ "Day 2:" ++ solveDay2
  putStrLn $ "Day 3:" ++ solveDay3
  putStrLn $ "Day 3.2:" ++ solveDay3_2
  putStrLn $ "Day 4:" ++ Day4.solve
  putStrLn $ "Day 4.2:" ++ Day4.solvePart2
  putStrLn $ "Day 5:" ++ Day5.solve
  putStrLn $ "Day 5.2:" ++ Day5.solvePart2
  putStrLn $ "Day 6:" ++ Day6.solve
  putStrLn $ "Day 6.2:" ++ Day6.solvePart2
  putStrLn $ "Day 7:" ++ Day7.solve
  putStrLn $ "Day 7.2:" ++ Day7.solvePart2
  putStrLn $ "Day 8:" ++ Day8.solve
  putStrLn "Day 8.2:"
  Day8.solvePart2
  putStrLn $ "Day 9:" ++ Day9.solve
  putStrLn $ "Day 9.2:" ++ Day9.solvePart2
  putStrLn $ "Day 10:" ++ Day10.solve
  putStrLn $ "Day 10.2:" ++ Day10.solvePart2
