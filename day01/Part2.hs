-- cat input2.txt | runhaskell Part2.hs

main :: IO ()
main = do
  measurements <- fmap read . lines <$> getContents
  print (increases measurements)

increases :: [Integer] -> Integer
increases (a:t@(x:y:b:ts))
  | b > a = 1 + increases t
  | otherwise = increases t
increases _ = 0
