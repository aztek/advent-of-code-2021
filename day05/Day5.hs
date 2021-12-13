{-# LANGUAGE TupleSections #-}

import Data.List (concatMap, group, sort)

main :: IO ()
main = do
  vents <- map readVent . lines <$> getContents
  -- let points = concatMap hvlines vents -- Part 1
  let points = concatMap hvdlines vents -- Part 2
  let dangers = [ head p | p <- group (sort points), length p > 1 ]
  print (length dangers)

type Point = (Integer, Integer)

readPoint :: ReadS Point
readPoint s = do
  (x, ',':s') <- readsPrec 0 s
  (y, s'') <- readsPrec 0 s'
  return ((x, y), s'')

type Vent = (Point, Point)

readVent :: String -> Vent
readVent s = head $ do
  (x, ' ':'-':'>':' ':s') <- readPoint s
  (y, s'') <- readPoint s'
  return (x, y)

hvlines :: Vent -> [Point]
hvlines ((x1, y1), (x2, y2))
  | x1 == x2  = map (x1,) ys
  | y1 == y2  = map (,y1) xs
  | otherwise = []
  where xs = [x1 `min` x2 .. x1 `max` x2]
        ys = [y1 `min` y2 .. y1 `max` y2]

hvdlines :: Vent -> [Point]
hvdlines ((x1, y1), (x2, y2))
  | horizontal   = zip (repeat x1) ys
  | vertical     = zip xs (repeat y1)
  | diagonal     = zip xs ys
  | antidiagonal = zip xs (reverse ys)
  | otherwise    = []
  where horizontal   = x1 == x2
        vertical     = y1 == y2
        diagonal     = x1 - x2 == y1 - y2
        antidiagonal = x1 - y2 == x2 - y1
        xs = [x1 `min` x2 .. x1 `max` x2]
        ys = [y1 `min` y2 .. y1 `max` y2]
