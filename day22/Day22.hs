{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  steps <- parse <$> getContents
  print (part1 steps)
  print (part2 steps)

part1 :: [Step] -> Int
part1 = length . filter id . Map.elems . reboot

part2 :: [Step] -> Int
part2 = sum . map lit . foldl (flip $ uncurry helper) []
  where helper True  = appendCuboid
        helper False = extractCuboid

        lit :: Cuboid -> Int
        lit ((x1, y1, z1), (x2, y2, z2)) = fromX (distance x1 x2) *
                                           fromY (distance y1 y2) *
                                           fromZ (distance z1 z2)
          where distance t1 t2 = t2 - t1 + 1

newtype X = X { fromX :: Int } deriving (Eq, Ord, Num, Enum)
newtype Y = Y { fromY :: Int } deriving (Eq, Ord, Num, Enum)
newtype Z = Z { fromZ :: Int } deriving (Eq, Ord, Num, Enum)

type Step = (Bool, Cuboid)
type Point = (X, Y, Z)
type Cuboid = (Point, Point)

parse :: String -> [Step]
parse = map parseLine . lines
  where
    parseLine s = (flag == "on", parseCuboids cuboids)
      where [flag, cuboids] = words s

    parseCuboids s = ((x1, y1, z1), (x2, y2, z2))
      where [x, y, z] = words (map (\c -> if c == ',' then ' ' else c) s)
            (x1, x2) = parseCuboid X x
            (y1, y2) = parseCuboid Y y
            (z1, z2) = parseCuboid Z z

    parseCuboid t (_:'=':s) = (t (read min), t (read max))
      where [min, max] = words (map (\c -> if c == '.' then ' ' else c) s)

type Grid = Map Point Bool

reboot :: [Step] -> Grid
reboot = foldl step (Map.fromList [(p, False) | p <- space])
  where space = tabulate ((-50, -50, -50), (50, 50, 50))

step :: Grid -> Step -> Grid
step grid (on, ranges) = foldr (Map.adjust (const on)) grid (tabulate ranges)

tabulate :: Cuboid -> [Point]
tabulate c = (,,) <$> interval cx <*> interval cy <*> interval cz
  where (cx, cy, cz) = edges c
        interval (t1, t2) = [t1 `max` (-50) .. 50 `min` t2]


-- * 1D

type Interval d = (d, d)

data Location = L | I | R
  deriving (Eq, Show, Ord)

locatePoint :: Ord d => d -> Interval d -> Location
locatePoint x (a, b) | x < a = L
                     | x <= b = I
                     | otherwise = R

within :: Ord d => Interval d -> Interval d -> Bool
(a, b) `within` c = locatePoint a c == I && locatePoint b c == I

cut :: (Num d, Ord d) => Interval d -> Interval d -> [Interval d]
cut (a, d) (b, c) = filter (uncurry (<=)) $
  case (locatePoint b (a, d), locatePoint c (a, d)) of
    (L, L) -> [(a, d)]
    (L, I) -> [(a, c), (c + 1, d)]
    (L, R) -> [(a, d)]
    (I, I) -> [(a, b - 1), (b, c), (c + 1, d)]
    (I, _) -> [(a, b - 1), (b, d)]
    (R, _) -> [(a, d)]


-- * 3D

edges :: Cuboid -> (Interval X, Interval Y, Interval Z)
edges ((x1, y1, z1), (x2, y2, z2)) = ((x1, x2), (y1, y2), (z1, z2))

inside :: Cuboid -> Cuboid -> Bool
a `inside` b = ax `within` bx && ay `within` by && az `within` bz
  where (ax, ay, az) = edges a
        (bx, by, bz) = edges b

diffCuboids :: Cuboid -> Cuboid -> [Cuboid]
diffCuboids a b = [c | c <- cuboids, not (c `inside` b)]
  where cuboids = cuboid <$> cut ax bx <*> cut ay by <*> cut az bz
        (ax, ay, az) = edges a
        (bx, by, bz) = edges b

cuboid :: Interval X -> Interval Y -> Interval Z -> Cuboid
cuboid (x1, x2) (y1, y2) (z1, z2) = ((x1, y1, z1), (x2, y2, z2))

appendCuboid :: Cuboid -> [Cuboid] -> [Cuboid]
appendCuboid c cs = c : extractCuboid c cs

extractCuboid :: Cuboid -> [Cuboid] -> [Cuboid]
extractCuboid c = concatMap (`diffCuboids` c)
