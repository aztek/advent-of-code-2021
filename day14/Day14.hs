{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.State (gets, modify, evalState, State)
import Data.Bifunctor (second)
import Data.Map (Map, (!))
import qualified Data.Map as Map

main :: IO ()
main = do
  (template, insertions) <- parse <$> getContents
  print (part1 insertions template)
  print (part2 insertions template)


-- * Common

type Template = [Char]
type Insertions = Map (Char, Char) Char

parse :: String -> (Template, Insertions)
parse = second (Map.fromList . map parseInsertion) . splitInput . lines
  where splitInput (template:[]:insertions) = (template, insertions)
        parseInsertion [a, b, ' ', '-', '>', ' ', c] = ((a, b), c)

answer :: Map Char Integer -> Integer
answer occurrences = maximum quantities - minimum quantities
  where quantities = Map.elems occurrences


-- * Part 1

part1 :: Insertions -> Template -> Integer
part1 insertions template = answer (occurrences template')
  where template' = iterate (step insertions) template !! 10
        occurrences = foldr (\c -> Map.insertWith (+) c 1) Map.empty

step :: Insertions -> Template -> Template
step is (h:t) = h : concatMap insert (zip (h:t) t)
  where insert (a, b) = [is ! (a, b), b]


-- * Part 2

type Memo a b = State (Map a b) b
type a ~> b = a -> Memo a b

memoize :: Ord a => (a -> Memo a b) -> a ~> b
memoize f a = gets (Map.lookup a) >>= \case
  Just b -> return b
  Nothing -> do b <- f a
                modify (Map.insert a b)
                return b

part2 :: Insertions -> Template -> Integer
part2 is = answer . evalMemo . occurrences
  where
    evalMemo memo = evalState memo Map.empty
    occurrences = fmap (Map.unionsWith (+)) . mapM (curry occs 40) . pairs
    -- 'occs (n, p)' calculates the statistics on the occurrences of proteins
    -- after 'n' steps of applying the insertions on a single pair 'p'
    occs :: (Int, (Char, Char)) ~> Map Char Integer
    occs = memoize $ \case
      (0, (_, b)) -> return (Map.singleton b 1)
      (n, (a, b)) -> Map.unionWith (+) <$> occs (n - 1, (a, c))
                                       <*> occs (n - 1, (c, b))
        where c = is ! (a, b)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)
