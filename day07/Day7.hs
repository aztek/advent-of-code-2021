
import Data.List (minimumBy)
import Data.Ord (comparing)

main :: IO ()
main = do
  positions <- readInput <$> getContents
  print (minimalCost cost' positions)

readInput :: String -> [Int]
readInput = map read . words
          . map (\c -> if c == ',' then ' ' else c)

minimalCost :: (Int -> Int -> Int) -> [Int] -> Int
minimalCost cost positions = snd (minimumBy (comparing snd) costs)
  where costs = [(p, sum (map (cost p) positions)) | p <- [0..length positions]]

-- Part 1

cost :: Int -> Int -> Int
cost p x = abs (x - p)

-- Part 2

cost' :: Int -> Int -> Int
cost' p x = (n * (n + 1)) `div` 2 where n = abs (x - p)
