{-# LANGUAGE LambdaCase #-}

import Data.List (sort)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  inputs <- lines <$> getContents
  let illegals = mapMaybe (incomplete []) inputs
  -- print $ sum (map score illegals) -- Part 1
  print $ middle (map score' illegals) -- Part 2

corrupt :: [Char] -> [Char] -> Maybe Char
corrupt _          []     = Nothing
corrupt stack      (c:cs) | c `elem` "([{<" = corrupt (c:stack) cs
corrupt []         (c:_)  = Just c
corrupt (s:stack') (c:cs) | match s == c = corrupt stack' cs
                         | otherwise = Just c

incomplete :: [Char] -> [Char] -> Maybe [Char]
incomplete []         []     = Nothing
incomplete stack      []     = Just (map match stack)
incomplete stack      (c:cs) | c `elem` "([{<" = incomplete (c:stack) cs
incomplete []         (c:_)  = Nothing
incomplete (s:stack') (c:cs) | match s == c = incomplete stack' cs
                             | otherwise = Nothing

match :: Char -> Char
match = \case
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'

score :: Char -> Int
score = \case
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137

score' :: [Char] -> Int
score' = foldl (\s c -> c + 5 * s) 0 . map braceScore
  where braceScore = \case
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4

middle :: Ord a => [a] -> a
middle xs = sort xs !! (length xs `div` 2)
