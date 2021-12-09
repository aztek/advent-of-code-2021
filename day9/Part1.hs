import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  heightmap <- readHeightmap . lines <$> getContents
  print $ sum [1 + (heightmap !! x !! y) | (x, y) <- lowerpoints heightmap]

readHeightmap :: [String] -> [[Int]]
readHeightmap = map (map digitToInt)

lowerpoints :: [[Int]] -> [(Int, Int)]
lowerpoints heightmap = filter (isLowerpoint heightmap)
                        [(x, y) | x <- [0..height], y <- [0..width]]
  where height = length heightmap - 1
        width = length (head heightmap) - 1

isLowerpoint :: Ord a => [[a]] -> (Int, Int) -> Bool
isLowerpoint heightmap (x, y) = all (> heightmap !! x !! y) adjacents
  where adjacents = mapMaybe (heightmap !??)
                             [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

(!?) :: [a] -> Int -> Maybe a
xs !? i
  | i >= 0 && i < length xs = Just (xs !! i)
  | otherwise = Nothing

(!??) :: [[a]] -> (Int, Int) -> Maybe a
xs !?? (x, y) = do { ys <- xs !? x; ys !? y }
