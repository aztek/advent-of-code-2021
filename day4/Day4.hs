{-# LANGUAGE TupleSections #-}

import Data.List (delete, find, inits, transpose)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)

main :: IO ()
main = do
  (numbers, boards) <- parse . lines <$> getContents
  let (round, board) = fromJust (winner boards numbers)
  -- let (round, board) = fromJust (loser boards numbers) -- Part 2
  print (score round board)

type Board = [[Integer]]

parse :: [String] -> ([Integer], [Board])
parse (ns:[]:bs) = (numbers, boards)
  where numbers = fmap read (split ',' ns)
        boards = fmap parseBoard (split [] bs)
        parseBoard = fmap (fmap read . words)

split :: Eq a => a -> [a] -> [[a]]
split sep [] = []
split sep lst | (chunk, rest) <- span (/= sep) lst =
  chunk : case rest of
            [] -> []
            _:lst' -> split sep lst'

winner :: [Board] -> [Integer] -> Maybe ([Integer], Board)
winner boards = search winning . inits
  where winning round = fmap (round,) (find (wins round) boards)

search :: (a -> Maybe b) -> [a] -> Maybe b
search f = listToMaybe . mapMaybe f

wins :: [Integer] -> Board -> Bool
wins round board = winsRow board || winsCol board
  where winsRow = any (all (`elem` round))
        winsCol = winsRow . transpose

score :: [Integer] -> Board -> Integer
score round board = last round * sum (unmarked round board)

unmarked :: [Integer] -> Board -> [Integer]
unmarked round = concatMap (filter (`notElem` round))

loser :: [Board] -> [Integer] -> Maybe ([Integer], Board)
loser boards numbers = do
  (round, board) <- winner boards numbers
  case delete board boards of
    [] -> return (round, board)
    boards' -> loser boards' numbers
