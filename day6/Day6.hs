{-# LANGUAGE LambdaCase #-}

import Control.Monad.State.Lazy (evalState, gets, liftM2, modify, State)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybe, listToMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let days = maybe 80 read (listToMaybe args)
  fishes <- readInput <$> getContents
  print (offsprings days fishes)

readInput :: String -> [Int]
readInput = map read . words
          . map (\c -> if c == ',' then ' ' else c)

offsprings :: Int -> [Int] -> Int
offsprings days = sum . eval . mapM (curry memoOffsprings days)
  where eval st = evalState st Map.empty

type Memo a b = State (Map a b) b

memoOffsprings :: (Int, Int) -> Memo (Int, Int) Int
memoOffsprings = memoize $ \case
  (0,    _)    -> return 1
  (days, 0)    -> liftM2 (+) (memoOffsprings (days - 1, 6))
                             (memoOffsprings (days - 1, 8))
  (days, fish) -> memoOffsprings (days - 1, fish - 1)

memoize :: Ord a => (a -> Memo a b) -> a -> Memo a b
memoize f a = do
  res <- gets (Map.lookup a)
  case res of
    Just b -> return b
    Nothing -> do b <- f a
                  modify (Map.insert a b)
                  return b
