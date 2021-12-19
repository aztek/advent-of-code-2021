import Prelude hiding (Left, Right)
import Control.Applicative ((<|>))

main :: IO ()
main = do
  snailfishes <- parse <$> getContents
  print (part1 snailfishes)
  print (part2 snailfishes)

part1 :: [Snailfish] -> Int
part1 = magnitude . foldl1 plus

part2 :: [Snailfish] -> Int
part2 snailfishes = maximum (map (magnitude . uncurry plus) pairs)
  where pairs = concatMap allBut snailfishes
        allBut s = [(s, t) | t <- snailfishes, t /= s]


-- * Parsing and pretty printing

data Snailfish
  = Number Int
  | Pair Snailfish Snailfish
  deriving (Eq)

instance Read Snailfish where
  readsPrec p ('[':s) = do (a, ',':s')  <- readsPrec p s
                           (b, ']':s'') <- readsPrec p s'
                           return (Pair a b, s'')
  readsPrec p s = do (n, s') <- readsPrec p s
                     return (Number n, s')

parse :: String -> [Snailfish]
parse = map read . lines

instance Show Snailfish where
  show (Number n) = show n
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"


-- * Snailfish zipper

data Context
  = Top
  | Left Context Snailfish
  | Right Snailfish Context
  deriving (Eq, Show, Read)

type Location = (Snailfish, Context)

left :: Location -> Location
left (Pair l r, c) = (l, Left c r)

right :: Location -> Location
right (Pair l r, c) = (r, Right l c)

top :: Snailfish -> Location
top t = (t, Top)

up :: Location -> Location
up (t, Left c r) = (Pair t r, c)
up (t, Right l c) = (Pair l t, c)

leftmost :: Location -> Location
leftmost loc@(Number{}, _) = loc
leftmost loc@(Pair{}, _) = leftmost (left loc)

rightmost :: Location -> Location
rightmost loc@(Number{}, _) = loc
rightmost loc@(Pair{}, _) = rightmost (right loc)

upmost :: Location -> Location
upmost (t, Top) = (t, Top)
upmost l = upmost (up l)

modify :: (Snailfish -> Snailfish) -> Location -> Location
modify f (s, c) = (f s, c)

rightValue :: Location -> Maybe Location
rightValue (_, Top) = Nothing
rightValue loc@(_, Right{}) = rightValue (up loc)
rightValue loc@(_, Left{}) = Just (leftmost (right (up loc)))

leftValue :: Location -> Maybe Location
leftValue (_, Top) = Nothing
leftValue loc@(_, Left{}) = leftValue (up loc)
leftValue loc@(_, Right{}) = Just (rightmost (left (up loc)))


-- * Magnitude

magnitude :: Snailfish -> Int
magnitude (Number n) = n
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r


-- * Reductions

plus :: Snailfish -> Snailfish -> Snailfish
plus a b = repeatedly reduce (Pair a b)
  where
    repeatedly :: (a -> Maybe a) -> a -> a
    repeatedly f a | Just a' <- f a = repeatedly f a'
                   | otherwise = a

    reduce :: Snailfish -> Maybe Snailfish
    reduce s = explode s <|> split s

explode :: Snailfish -> Maybe Snailfish
explode = chain (map (update (findNested 4))
                     [modifyLeft, modifyRight, modifySelf])
  where
    chain :: Monad m => [a -> m a] -> a -> m a
    chain fs a = foldl (>>=) (return a) fs

    findNested :: Int -> Location -> Maybe Location
    findNested 0 loc@(Pair Number{} Number{}, _) = Just loc
    findNested n (Number{}, _) = Nothing
    findNested n loc@(Pair{}, _) =  findNested (n-1) (left loc)
                                <|> findNested (n-1) (right loc)

    modifyLeft :: Location -> Location
    modifyLeft loc@(Pair (Number a) _, _) =
      case leftValue loc of
        Just l -> modify (\(Number n) -> Number (a + n)) l
        Nothing -> loc

    modifyRight :: Location -> Location
    modifyRight loc@(Pair _ (Number b), _) =
      case rightValue loc of
        Just r -> modify (\(Number n) -> Number (n + b)) r
        Nothing -> loc

    modifySelf :: Location -> Location
    modifySelf = modify (\_ -> Number 0)

split :: Snailfish -> Maybe Snailfish
split = update findSplittable (modify split')
  where
    findSplittable :: Location -> Maybe Location
    findSplittable loc@(Number n, _) = if n > 9 then Just loc else Nothing
    findSplittable loc@(Pair{}, _) =  findSplittable (left loc)
                                  <|> findSplittable (right loc)

    split' :: Snailfish -> Snailfish
    split' (Number n) = Pair (Number $ floor k) (Number $ ceiling k)
      where k = fromIntegral n / 2

update :: (Location -> Maybe Location) -> (Location -> Location)
       -> Snailfish -> Maybe Snailfish
update find modify = fmap (fst . upmost . modify) . find . top
