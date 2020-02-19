module Day10
  ( solve
  , solvePart2
  ) where

import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Debug.Trace

solve = show $ best (toMap dayInput)

solvePart2 = show $ solveForPart2 (toMap dayInput)

solveForPart2 field =
  let th = sorted !! 199
   in fst th * 100 + snd th
  where
    station = best field
    stationPos = fst station
    asteroidPos = M.keys $ getAsteroids field
    sorted = sortBy (compareAsteroids field stationPos) asteroidPos

compareAsteroids :: M.Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Ordering
compareAsteroids field center@(xc, yc) a1@(x1, y1) a2@(x2, y2)
  | a1 == a2 = EQ
  | los1 && not los2 = LT
  | not los1 && los2 = GT
  | los1 == los2 = compareClockwise a1 a2
  | otherwise = error "compare error"
  where
    los1 = hasLOS field center a1
    los2 = hasLOS field center a2
    line = expand bounds a1 a2
    maxP = max a1 a2
    minP = min a1 a2
    bounds = traceShow ("bounds:" ++ show (fst $ M.findMax field)) fst $ M.findMax field
    compareClockwise a b = compare (relAngle center a) (relAngle center b)

at :: [(Int, Int)]
at = [(1, 0), (2, 0), (2, 1), (2, 2), (1, 2), (0, 2), (0, 1), (0, 0)]

relAngle :: (Int, Int) -> (Int, Int) -> Float
relAngle c@(cx, cy) p@(absx, absy)
  | x >= 0 && y >= 0 = angle -- first quadrant
  | x == 0 && y < 0 = 180
  | x > 0 && y < 0 = 90 + angle -- second quadrant
  | x < 0 && y < 0 = 180 - angle -- third quadrant
  | x < 0 && y >= 0 = 360 + angle --fourth quadrant
  | otherwise = error "relAngle error"
  where
    x = absx - cx
    y = cy - absy
    r2 = (-x) ^ 2 + y ^ 2
    r = sqrt (fromIntegral r2)
    angle = (180 / pi) * asin (fromIntegral x / r)

toDegree rad = 180 / pi * rad

best :: M.Map (Int, Int) Char -> ((Int, Int), Int)
best field =
  let asteroidPos = M.keys $ getAsteroids field
      loses = M.fromList [((a, a2), hasLOS field a a2) | a <- asteroidPos, a2 <- asteroidPos, a < a2]
   in maximumBy
        (\(p1, s1) (p2, s2) -> compare s1 s2)
        (map (\a -> (a, length $ filter (\x -> a /= x && loses M.! (min a x, max a x)) asteroidPos)) asteroidPos)

--has line of sight
hasLOS :: M.Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Bool
hasLOS map p1@(x1, y1) p2@(x2, y2) = (p1 /= p2) && all (not . isAsteroid) between
  where
    between = [x | x <- line, x > minP, x < maxP]
    isAsteroid coord =
      case M.lookup coord map of
        Just x  -> x == '#'
        Nothing -> error $ "coord not found " ++ show coord
    maxP = max p1 p2
    minP = min p1 p2
    bounds = fst $ M.findMax map
    line = expand bounds p1 p2

-- get all points touched by the line defined by this two points
expand :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
expand (maxX, maxY) p1@(x1, y1) p2@(x2, y2) =
  case nextP of
    Just x -> expand (maxX, maxY) p1 x
    _ -> sort $ drop 2 $ expand' (maxX, maxY) p1 p2 ++ expand' (maxX, maxY) p2 p1
  where
    gcdDiff = gcd (x2 - x1) (y2 - y1)
    nextP
      | gcdDiff > 0 && gcdDiff /= 1 = Just (x1 + ((x2 - x1) `div` gcdDiff), y1 + ((y2 - y1) `div` gcdDiff))
      | otherwise = Nothing

--expand single direction
expand' :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
expand' (maxX, maxY) p1@(x1, y1) p2@(x2, y2)
  | valid nextP = p1 : expand' (maxX, maxY) p2 nextP
  | otherwise = [p1, p2]
  where
    diff = (x2 - x1, y2 - y1)
    nextP = (x2 + fst diff, y2 + snd diff)
    valid (x, y) = x >= 0 && y >= 0 && x <= maxX && y <= maxY

getAsteroids :: M.Map (Int, Int) Char -> M.Map (Int, Int) Char
getAsteroids = M.filter (== '#')

toMap :: [String] -> M.Map (Int, Int) Char
toMap str = M.fromList . concat $ map rowToPoints rows
  where
    rows :: [(Int, String)]
    rows = zip [0 ..] str
    rowToPoints :: (Int, String) -> [((Int, Int), Char)]
    rowToPoints (y, colsStr) = zipWith (\c x -> ((x, y), c)) colsStr [0 ..]

--Best is 5,8 with 33 other asteroids detected:
test1 =
  [ "......#.#."
  , "#..#.#...."
  , "..#######."
  , ".#.#.###.."
  , ".#..#....."
  , "..#....#.#"
  , "#..#....#."
  , ".##.#..###"
  , "##...#..#."
  , ".#....####"
  ]

--Best is 1,2 with 35 other asteroids detected:
test2 =
  [ "#.#...#.#."
  , ".###....#."
  , ".#....#..."
  , "##.#.#.#.#"
  , "....#.#.#."
  , ".##..###.#"
  , "..#...##.."
  , "..##....##"
  , "......#..."
  , ".####.###."
  ]

--Best is 6,3 with 41 other asteroids detected:
test3 =
  [ ".#..#..###"
  , "####.###.#"
  , "....###.#."
  , "..###.##.#"
  , "##.##.#.#."
  , "....###..#"
  , "..#.#..#.#"
  , "#..#.#.###"
  , ".##...##.#"
  , ".....#.#.."
  ]

--Best is 11,13 with 210 other asteroids detected:
--The 200th asteroid to be vaporized is at 8,2.
test4 =
  [ ".#..##.###...#######"
  , "##.############..##."
  , ".#.######.########.#"
  , ".###.#######.####.#."
  , "#####.##.#.##.###.##"
  , "..#####..#.#########"
  , "####################"
  , "#.####....###.#.#.##"
  , "##.#################"
  , "#####.##.###..####.."
  , "..######..##.#######"
  , "####.##.####...##..#"
  , ".#####..#.######.###"
  , "##...#.##########..."
  , "#.##########.#######"
  , ".####.#.###.###.#.##"
  , "....##.##.###..#####"
  , ".#.#.###########.###"
  , "#.#.#.#####.####.###"
  , "###.##.####.##.#..##"
  ]

dayInput =
  [ "..#..###....#####....###........#"
  , ".##.##...#.#.......#......##....#"
  , "#..#..##.#..###...##....#......##"
  , "..####...#..##...####.#.......#.#"
  , "...#.#.....##...#.####.#.###.#..#"
  , "#..#..##.#.#.####.#.###.#.##....."
  , "#.##...##.....##.#......#.....##."
  , ".#..##.##.#..#....#...#...#...##."
  , ".#..#.....###.#..##.###.##......."
  , ".##...#..#####.#.#......####....."
  , "..##.#.#.#.###..#...#.#..##.#...."
  , ".....#....#....##.####....#......"
  , ".#..##.#.........#..#......###..#"
  , "#.##....#.#..#.#....#.###...#...."
  , ".##...##..#.#.#...###..#.#.#..###"
  , ".#..##..##...##...#.#.#...#..#.#."
  , ".#..#..##.##...###.##.#......#..."
  , "...#.....###.....#....#..#....#.."
  , ".#...###..#......#.##.#...#.####."
  , "....#.##...##.#...#........#.#..."
  , "..#.##....#..#.......##.##.....#."
  , ".#.#....###.#.#.#.#.#............"
  , "#....####.##....#..###.##.#.#..#."
  , "......##....#.#.#...#...#..#....."
  , "...#.#..####.##.#.........###..##"
  , ".......#....#.##.......#.#.###..."
  , "...#..#.#.........#...###......#."
  , ".#.##.#.#.#.#........#.#.##..#..."
  , ".......#.##.#...........#..#.#..."
  , ".####....##..#..##.#.##.##..##..."
  , ".#.#..###.#..#...#....#.###.#..#."
  , "............#...#...#.......#.#.."
  , ".........###.#.....#..##..#.##..."
  ]
