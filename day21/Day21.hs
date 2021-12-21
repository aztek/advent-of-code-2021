{-# LANGUAGE LambdaCase #-}

import Control.Monad.State (put, get, gets, modify, evalState, runState, State)
import Data.Bifunctor (second)

import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  (n, m) <- parse <$> getContents
  print (part1 n m)
  print (part2 n m)

part1 :: Int -> Int -> Int
part1 n m = rolls * (score alice `min` score bob)
  where ((alice, bob), rolls) = runDice $ play (Player n 0) (Player m 0)

part2 :: Int -> Int -> Int
part2 n m = a `max` b
  where (a, b) = wins (Player n 0, Player m 0)


-- * Common

data Player = Player {
  pos :: Int,
  score :: Int
} deriving (Eq, Show, Ord)

parse :: String -> (Int, Int)
parse str = (pos n, pos m)
  where [n, m] = lines str
        pos = read . drop (length "Player X starting position: ")

place :: Int -> Int
place n | n <= 10 = n
        | otherwise = place (n - 10)

advance :: Player -> Int -> Player
advance (Player pos score) r = Player pos' (score + pos')
  where pos' = place (pos + r)


-- * Part 1

type Dice a = State (Int, [Int]) a

runDice :: Dice a -> (a, Int)
runDice st = second fst (runState st (0, cycle [1..100]))

roll :: Int -> Dice Int
roll n = do
  (rolls, dice) <- get
  let (rs, dice') = splitAt n dice
  put (rolls + n, dice')
  return (sum rs)

move :: Player -> Dice Player
move player = advance player <$> roll 3

play :: Player -> Player -> Dice (Player, Player)
play alice bob = do
  alice' <- move alice
  if score alice' >= 1000
  then return (alice', bob)
  else do
    bob' <- move bob
    if score bob' >= 1000
    then return (alice', bob')
    else play alice' bob'


-- * Part 2

type Memo a b = State (Map a b) b

evalMemo :: Memo a b -> b
evalMemo st = evalState st Map.empty

memoize :: Ord a => (a -> Memo a b) -> a -> Memo a b
memoize f a = do
  res <- gets (Map.lookup a)
  case res of
    Just b -> return b
    Nothing -> do b <- f a
                  modify (Map.insert a b)
                  return b

wins :: (Player, Player) -> (Int, Int)
wins = evalMemo . curry wins' True
  where
    wins' :: (Bool, (Player, Player)) -> Memo (Bool, (Player, Player)) (Int, Int)
    wins' = memoize $ \case
      (turn, (alice, bob))
        | score alice >= 21 -> return (1, 0)
        | score bob   >= 21 -> return (0, 1)
        | otherwise -> foldl1 plus <$> mapM helper multirolls
        where
          helper :: Int -> Memo (Bool, (Player, Player)) (Int, Int)
          helper roll = wins' (not turn, players)
            where players = if turn
                            then (advance alice roll, bob)
                            else (alice, advance bob roll)

plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus (a, b) (x, y) = (a + x, b + y)

multirolls :: [Int]
multirolls = [a + b + c | a <- [1..3], b <- [1..3], c <- [1..3]]
