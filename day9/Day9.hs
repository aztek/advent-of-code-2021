import Data.Char (digitToInt, intToDigit)
import Data.Maybe (mapMaybe)
import Data.List (nub, sort)

main :: IO ()
main = do
  heightmap <- readHeightmap . lines <$> getContents
  -- print (part1 heightmap)
  print (part2 heightmap)

part1 :: [[Int]] -> Int
part1 heightmap = sum . map height $ lowerpoints heightmap
  where height (x, y) = 1 + heightmap !! x !! y

part2 :: [[Int]] -> Int
part2 heightmap = product . take 3 . reverse . sort
                . map (length . basin heightmap)
                $ lowerpoints heightmap

readHeightmap :: [String] -> [[Int]]
readHeightmap = map (map digitToInt)

printHeightmap :: [[Char]] -> IO ()
printHeightmap = mapM_ putStrLn

lowerpoints :: [[Int]] -> [(Int, Int)]
lowerpoints heightmap = filter (isLowerpoint heightmap)
                        [(x, y) | x <- [0..height], y <- [0..width]]
  where height = length heightmap - 1
        width = length (head heightmap) - 1

isLowerpoint :: Ord a => [[a]] -> (Int, Int) -> Bool
isLowerpoint heightmap (x, y) = all (> heightmap !! x !! y) adjacents
  where adjacents = mapMaybe (heightmap !??) (neighbors (x, y))

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

(!?) :: [a] -> Int -> Maybe a
xs !? i
  | i >= 0 && i < length xs = Just (xs !! i)
  | otherwise = Nothing

(!??) :: [[a]] -> (Int, Int) -> Maybe a
xs !?? (x, y) = do { ys <- xs !? x; ys !? y }

basin :: [[Int]] -> (Int, Int) -> [(Int, Int)]
basin heightmap x = converge (growBasin heightmap) [x]

converge :: Eq a => (a -> a) -> a -> a
converge f a = if a == a' then a else converge f a' where a' = f a

growBasin :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
growBasin heightmap basin = nub $ sort (basin ++ concatMap grow basin)
  where grow = filter falling . neighbors
        falling p = case heightmap !?? p of
          Just h  -> h /= 9
          Nothing -> False
