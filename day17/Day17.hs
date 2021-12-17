import Control.Monad (liftM2)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (transpose, maximumBy)

main :: IO ()
main = do
  target@((xin, xax), (yin, yax)) <- parse <$> getContents
  let searchSpace = liftM2 (,) [0..xax] [-200..200] -- ¯\_(ツ)_/¯
  let successfulShots = filter (hits target) searchSpace
  let (dx, dy) = maximumBy (compare `on` snd) successfulShots
  print (highestY dy) -- Part 1
  print (length successfulShots) -- Part 2

type X = Int
type Y = Int

type Target = ((X, X), (Y, Y))

parse :: String -> Target
parse str = (parseRange xs, parseRange ys)
  where [xs, ys] = words (drop (length "target area: ") (filter (/= ',') str))
        parseRange (_coord:'=':s) = (read x, read y)
          where [x, y] = words (map (\c -> if c == '.' then ' ' else c) s)

type DX = Int
type DY = Int

step :: (X, Y) -> (DX, DY) -> ((X, Y), (DX, DY))
step (x, y) (dx, dy) = ((x + dx, y + dy), (drag dx, dy - 1))
  where drag 0 = 0
        drag x = if x > 0 then x-1 else x+1

highestY :: DY -> Int
highestY dy = dy * (dy + 1) `div` 2

boxed :: Target -> (X, Y) -> Bool
boxed ((xin, xax), (yin, yax)) (x, y) =
  xin <= x && x <= xax && yin <= y && y <= yax

hits :: Target -> (DX, DY) -> Bool
hits target@((xin, xax), (yin, yax)) = hit (0, 0)
  where hit (x, y) (dx, dy)
          | boxed target (x, y) = True
          | x > xax && dx >= 0 = False
          | x < xin && dx <= 0 = False
          | y < yin && dy <= 0 = False
          | otherwise = uncurry hit $ step (x, y) (dx, dy)

{-

Ended up not using these:

-- The value of x at a given step.
x :: DX -> Int -> X
x vel step = if vel > 0 then m else -m
  where m = step' * vel' - (step' * (step' - 1)) `div` 2
        step' = step `min` vel'
        vel' = abs vel

-- The value of y at a given step.
y :: DY -> Int -> Y
y vel step = step * vel - (step * (step - 1)) `div` 2

visualize :: [(X, Y)] -> Target -> IO ()
visualize cs target@((xin, xax), (yin, yax)) = mapM_ (putStrLn . map point) points
  where points = [[(x,y) | x <- xs'] | y <- reverse ys']
        xs' = [minimum xs `min` xin .. maximum xs `max` xax]
        ys' = [minimum ys `min` yin .. maximum ys `max` yax]
        (xs, ys) = unzip cs
        point p
          | p `elem` cs = '#'
          | boxed target p = 'T'
          | otherwise = '.'

-}