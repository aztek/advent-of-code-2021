{-# LANGUAGE LambdaCase #-}

import Control.Monad.State (gets, modify, evalState, State)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt, intToDigit)
import Data.Map (Map, (!))
import qualified Data.Map as Map

main :: IO ()
main = do
  octo <- parse <$> getContents
  print (part1 100 octo)

part1 :: Int -> Octopodes -> Int
part1 steps = sum . map countZeros . take (1 + steps) . iterate step
  where countZeros = length . filter (== 0) . Map.elems

type Energy = Int
type Coord = (Int, Int)
type Octopodes = Map Coord Energy

parse :: String -> Octopodes
parse str = Map.fromList [ ((x, y), levels !! x !! y)
                         | x <- [0..width-1], y <- [0..height-1] ]
  where levels = map (map digitToInt) (lines str)
        width  = length levels
        height = length (head levels)

dimentions :: Octopodes -> (Int, Int)
dimentions = bimap dimension dimension . unzip . Map.keys
  where dimension xs = maximum xs + 1

step :: Octopodes -> Octopodes
step octo = evalState (stepS octo (Map.keys octo)) []

stepS :: Octopodes -> [Coord] -> State [Coord] Octopodes
stepS octo [] = return octo
stepS octo (r:rs) = raise octo r >>= \case
  Nothing -> stepS octo rs
  Just (e, rs') -> stepS (Map.adjust (const e) r octo) (rs' ++ rs)

raise :: Octopodes -> Coord -> State [Coord] (Maybe (Energy, [Coord]))
raise octo r = gets (r `elem`) >>= \case
  True  -> return Nothing
  False -> case octo ! r of
             9 -> modify (r:) >> return (Just (0, adjacent octo r))
             e -> return (Just (e + 1, []))

adjacent :: Octopodes -> Coord -> [Coord]
adjacent octo = filter valid . box
  where (height, width) = dimentions octo
        valid (x, y) = x >= 0 && x < width && y >= 0 && y < height
        box (x, y) = [ (x + dx, y + dy)
                     | dx <- [-1, 0, 1]
                     , dy <- [-1, 0, 1]
                     , (dx, dy) /= (0, 0) ]
