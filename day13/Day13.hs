{-# LANGUAGE LambdaCase #-}

import Data.Bifunctor (bimap)
import Data.Maybe (fromJust)
import Data.List (elemIndex, isPrefixOf, transpose)

main :: IO ()
main = do
  (paper, folds) <- parse <$> getContents
  -- part1 paper folds
  part2 paper folds

part1 :: Paper -> [Fold] -> IO ()
part1 paper = print . length . filter id . concat . foldPaper paper . head

part2 :: Paper -> [Fold] -> IO ()
part2 paper = printPaper . foldl foldPaper paper

data Fold
  = X Int
  | Y Int
  deriving (Eq, Show)

type Paper = [[Bool]]
type Manual = (Paper, [Fold])

parse :: String -> Manual
parse str = (render (map parseCoord coords), map parseFold folds)
  where (coords, folds) = split [] (lines str)
        parseCoord = bimap read read . split ','
        parseFold l = case drop (length "fold along ") l of
                        'x':'=':z -> X (read z)
                        'y':'=':z -> Y (read z)

split :: Eq a => a -> [a] -> ([a], [a])
split sep lst = (take index lst, drop (index + 1) lst)
  where index = fromJust (elemIndex sep lst)

render :: [(Int, Int)] -> Paper
render coords = transpose [[dot x y | y <- [0..width]] | x <- [0..height]]
  where (height, width) = bimap maximum maximum (unzip coords)
        dot x y = (x, y) `elem` coords

printPaper :: Paper -> IO ()
printPaper = mapM_ (putStrLn . map (\x -> if x then '#' else '.'))

foldPaper :: Paper -> Fold -> Paper
foldPaper paper fold = uncurry merge (divide fold paper)
  where merge = zipWith (zipWith (||))

divide :: Fold -> Paper -> (Paper, Paper)
divide = \case
  X x -> divideVertically x
  Y y -> divideHorizontally y

divideHorizontally :: Int -> Paper -> (Paper, Paper)
divideHorizontally y = uncurry align . splitAt y

align :: Paper -> Paper -> (Paper, Paper)
align top (_:bottom) = (pad top, reverse (pad bottom))
  where height = length top `max` length bottom
        pad p = p ++ replicate (height - length p)
                               (replicate (length (head p)) False)

divideVertically :: Int -> Paper -> (Paper, Paper)
divideVertically x = bimap transpose transpose
                   . divideHorizontally x . transpose
