{-# LANGUAGE LambdaCase #-}

import Control.Monad (filterM, when)
import Control.Monad.Reader (runReader, Reader, local, asks)
import Control.Monad.Writer (execWriterT, WriterT, tell)
import Data.Bifunctor (bimap)
import Data.Char (isUpper)
import Data.List (delete, elemIndex, intercalate, nub)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  maze <- parse <$> getContents
  print (part2 maze)

part1 :: Maze -> Int
part1 = length . flip paths Nothing

part2 :: Maze -> Int
part2 maze@(entrances, graph, exits) = length paths'
  where paths' = nub (concatMap (paths maze) (Nothing:exemptions))
        exemptions = map Just (filter (not . big) caves)
        caves = nub (entrances ++ Map.keys graph ++ exits)


-- * Caves

type Cave = String

big :: Cave -> Bool
big = all isUpper


-- * Graphs

type Graph a = Map a (Set a)

multinsert :: Ord a => a -> a -> Graph a -> Graph a
multinsert k v g = Map.insert k v' g
  where v' = if Map.member k g then Set.insert v (g ! k) else Set.singleton v

passages :: Ord a => Graph a -> a -> [a]
passages g a | Just p <- Map.lookup a g = Set.toList p
             | otherwise = []

-- * Mazes

-- | A list of entrances, the graph of passages between caves, and a list of exits.
type Maze = ([Cave], Graph Cave, [Cave])

parse :: String -> Maze
parse = foldr (extend . parseLine) mempty . lines
  where
    parseLine line = (take dash line, drop (dash + 1) line)
      where dash = fromJust (elemIndex '-' line)

    extend = \case
      ("start", to)   -> addEntrance to
      (from, "start") -> addEntrance from
      ("end", to)     -> addExit to
      (from, "end")   -> addExit from
      (from, to)      -> addPassage from to

    addEntrance :: Cave -> Maze -> Maze
    addEntrance e (entrances, graph, exits) = (e:entrances, graph, exits)

    addPassage :: Cave -> Cave -> Maze -> Maze
    addPassage from to (entrances, graph, exits) = (entrances, graph', exits)
      where graph' = multinsert from to (multinsert to from graph)

    addExit :: Cave -> Maze -> Maze
    addExit e (entrances, graph, exits) = (entrances, graph, e:exits)


-- * Traversals

type Path = [Cave]

-- | An auxillary monad stack for exploring all paths through a maze.
-- 'Writer' is for collecting the paths, 'Reader' is for keeping track
-- of visited caves ('Map Cave Int') and the current path ('Path').
type Traversal = WriterT [Path] (Reader (Map Cave Int, Path))

paths :: Maze -> Maybe Cave -> [Path]
paths (entrances, graph, exits) exemption = runTraversal (mapM_ explore entrances)
  where
    runTraversal :: Traversal () -> [Path]
    runTraversal t = runReader (execWriterT t) (Map.empty, [])

    explore :: Cave -> Traversal ()
    explore cave = enter cave $ do
      when (cave `elem` exits) savePath
      nexts <- filterM explorable (passages graph cave)
      mapM_ explore nexts

    savePath :: Traversal ()
    savePath = do { path <- asks snd; tell [path] }

    explorable :: Cave -> Traversal Bool
    explorable cave = asks $ \(caves, _) -> visits caves < limit
      where visits caves = fromMaybe 0 (caves !? cave)
            limit = if exemption == Just cave then 2 else 1

    enter :: Cave -> Traversal () -> Traversal ()
    enter cave = local $ bimap (visit cave) (cave:)

    visit :: Cave -> Map Cave Int -> Map Cave Int
    visit cave caves
      | big cave = caves
      | Map.member cave caves = Map.adjust (+1) cave caves
      | otherwise = Map.insert cave 1 caves
