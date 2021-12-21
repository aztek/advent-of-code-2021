import Control.Monad (liftM2, (>=>))
import Data.Bifunctor (second)
import Data.List (permutations)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  scanners <- parse <$> getContents
  let Just (ss, bs) = detects scanners
  print (part1 bs)
  print (part2 ss)

type Point = (Int, Int, Int)
type Scanner = Set Point
type Beacon = Point
type Beacons = Set Beacon

part1 :: Beacons -> Int
part1 = length

part2 :: [Point] -> Int
part2 ps = maximum [manhattan a b | a <- ps, b <- ps]

manhattan :: Point -> Point -> Int
manhattan (a, b, c) (x, y, z) = abs (a - x) + abs (b - y) + abs (c - z)


-- * Parsing

parse :: String -> NonEmpty Scanner
parse = NonEmpty.fromList . map parseScanner . split [] . lines
  where parseScanner = Set.fromList . map parsePoint . tail
        parsePoint s = (x, y, z)
          where [x, y, z] = map read (words (map commaSep s))
                commaSep c = if c == ',' then ' ' else c

split :: Eq a => a -> [a] -> [[a]]
split sep [] = []
split sep lst | (chunk, rest) <- span (/= sep) lst =
  chunk : case rest of
            [] -> []
            _:lst' -> split sep lst'


-- * Helpers

tmap :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
tmap (#) (x, y, z) (x', y', z') = (x # x', y # y', z # z')

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

heads :: Eq a => [a] -> [(a, [a])]
heads [] = []
heads (a:as) = (a, as) : [(b, a : filter (/= b) as) | b <- as]


-- * Orientations

type Orientation = Point -> Point

-- TODO: There should be 24 orientations instead
orientations :: [Orientation]
orientations = take 48 $ tabulate (rotate >=> reflect)

rotate :: Point -> [Point]
rotate (x, y, z) = [(x', y', z') | [x', y', z'] <- permutations [x, y, z]]

reflect :: Point -> [Point]
reflect p = [tmap (*) p r | r <- reflections]
  where reflections = [(i, j, k) | i <- [1, -1], j <- [1, -1], k <- [1, -1]]

tabulate :: (a -> [b]) -> [a -> b]
tabulate f = map (\i a -> f a !! i) [0..]

orient :: Scanner -> Orientation -> Scanner
orient s o = Set.map o s


-- * Detecting beacons

detects :: NonEmpty Scanner -> Maybe ([Point], Beacons)
detects = detects' [(0, 0, 0)]
  where
    detects' ps (s :| []) = return (ps, s)
    detects' ps (a :| bs) = firstJust helper (heads bs)
      where helper (b, bs) = do (p, c) <- detect a b
                                detects' (p:ps) (c :| bs)

detect :: Scanner -> Scanner -> Maybe (Point, Beacons)
detect a b = firstJust (align a . orient b) orientations

align :: Scanner -> Scanner -> Maybe (Point, Beacons)
align a b = fmap (second $ Set.union a) (recenter a b)

recenter :: Scanner -> Scanner -> Maybe (Point, Beacons)
recenter a b = firstJust valid centers
  where centers = liftM2 (tmap (-)) (Set.toList b) (Set.toList a)
        valid c | aligned a b' = Just (c, b')
                | otherwise = Nothing
          where b' = Set.map (norm c) b

norm :: Point -> Point -> Point
norm center point = tmap (-) point center

aligned :: Scanner -> Scanner -> Bool
aligned a b = length i >= 12 && not (any detectable d)
  where (i, d) = Set.partition (`Set.member` a) b

detectable :: Point -> Bool
detectable (x, y, z) = all (\t -> abs t <= 1000) [x, y, z]
