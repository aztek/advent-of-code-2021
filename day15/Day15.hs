import Control.Monad (liftM2)
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  matrix <- parseMatrix <$> getContents
  print (part1 matrix)
  print (part2 matrix)

part1 :: [[Int]] -> Int
part1 = minimalDanger . parseCavern

part2 :: [[Int]] -> Int
part2 = minimalDanger . parseCavern . expandCavern

type Position = (Int, Int)
data Cavern = Cavern {
  height :: Int,
  width  :: Int,
  risks  :: Map Position Int
} deriving (Show)

parseMatrix :: String -> [[Int]]
parseMatrix = map (map digitToInt) . lines

parseCavern :: [[Int]] -> Cavern
parseCavern matrix = Cavern height width risks
  where height = length matrix
        width  = length (head matrix)
        risks  = Map.fromList
               $ do (x, row)  <- zip [0..] matrix
                    (y, risk) <- zip [0..] row
                    return ((x, y), risk)

expandCavern :: [[Int]] -> [[Int]]
expandCavern = foldl1 juxtapose . explode . concat . explode
  where juxtapose = zipWith (++)
        explode = take 5 . iterate (map (map inc))
        inc x = if x + 1 > 9 then 1 else x + 1

minimalDanger :: Cavern -> Int
minimalDanger (Cavern height width risks) = minimalDangers ! (0, 0)
  where minimalDangers = dijkstra (Set.fromList positions) initDangers

        dijkstra :: Set Position -> Map Position Int -> Map Position Int
        dijkstra unseen dangers | Set.size unseen < 2 = dangers
        dijkstra unseen dangers = dijkstra unseen' dangers'
          where pos = minimumBy (compare `on` (dangers !)) unseen
                unseen'   = Set.delete pos unseen
                neighbors = filter (`elem` unseen) (adjacent pos)
                immediate = risks ! pos + dangers ! pos
                dangers'  = foldr (Map.adjust (min immediate)) dangers neighbors

        initDangers = Map.fromList $ zip positions (map initDanger positions)

        initDanger pos = if pos == (height-1, width-1) then 0 else maxBound

        positions = liftM2 (,) [0..height-1] [0..width-1]

        adjacent (x, y) = filter valid [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        valid (x, y) = x >= 0 && x < height && y >= 0 && y < width
