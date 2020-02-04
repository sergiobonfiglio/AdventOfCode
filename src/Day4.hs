module Day4
  ( solve
  , solvePart2
  ) where

solve :: String
solve = show $ length [x | x <- candidates, checkPassword x]

solvePart2 :: String
solvePart2 = show $ length [x | x <- candidates, checkPassword2 x]

candidates = map show [(fst input) .. (snd input)]

checkPassword2 :: String -> Bool
checkPassword2 x = increasingDigits x && hasDoubleAdjacentStrict x
  where
    hasDoubleAdjacentStrict [] = False
    hasDoubleAdjacentStrict str@(c1:rest) = length streak == 2 || hasDoubleAdjacentStrict (dropWhile (c1 ==) rest)
      where
        streak = takeWhile (c1 ==) str

checkPassword :: String -> Bool
checkPassword x = hasDoubleAdjacent x && increasingDigits x
  where
    hasDoubleAdjacent [x1, x2] = x1 == x2
    hasDoubleAdjacent (x1:x2:rest) = hasDoubleAdjacent [x1, x2] || hasDoubleAdjacent (x2 : rest)

increasingDigits [x1, x2] = x1 <= x2
increasingDigits (x1:x2:rest) = increasingDigits [x1, x2] && increasingDigits (x2 : rest)

input = (168630, 718098)
