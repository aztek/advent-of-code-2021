-- cat input2.txt | runhaskell Part2.hs
main :: IO ()
main = do
  steps <- fmap readStep . lines <$> getContents
  let Position hp depth aim = position steps
  print (hp * depth)

data Direction = Up | Down | Forward
data Step = Step Direction Integer

readStep :: String -> Step
readStep str = Step (readDirection d) (read s)
  where [d, s] = words str

readDirection :: String -> Direction
readDirection "up" = Up
readDirection "down" = Down
readDirection "forward" = Forward

data Position = Position {
  hp :: Integer,
  depth :: Integer,
  aim :: Integer
}

position :: [Step] -> Position
position = foldl move (Position 0 0 0)
  where
    move (Position hp depth aim) (Step d s) = case d of
      Up      -> Position hp depth (aim - s)
      Down    -> Position hp depth (aim + s)
      Forward -> Position (hp + s) (depth + aim * s) aim
