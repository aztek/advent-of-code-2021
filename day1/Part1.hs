-- cat input1.txt | runhaskell Part1.hs

main :: IO ()
main = do
  measurements <- fmap read . lines <$> getContents
  print (increases measurements)

increases :: [Integer] -> Integer
increases (x:y:ys)
  | y > x = 1 + increases (y:ys)
  | otherwise = increases (y:ys)
increases _ = 0
