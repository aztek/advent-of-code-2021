-- cat input2.txt | runhaskell Part2.hs

main :: IO ()
main = do
  let parseDiagnostic = map (== '1')
  report <- fmap parseDiagnostic . lines <$> getContents
  let (oxy, co2) = ratings report
  print (oxy * co2)

ratings :: [[Bool]] -> (Integer, Integer)
ratings report = (toNumber oxy, toNumber co2)
  where oxy = rating frequentBit report
        co2 = rating (not . frequentBit) report

rating :: ([Bool] -> Bool) -> [[Bool]] -> [Bool]
rating criterion [bits] = bits
rating criterion report = bit : bits
  where bit = criterion (map head report)
        bits = rating criterion [rs | (r:rs) <- report, r == bit]

frequentBit :: [Bool] -> Bool
frequentBit bits = disparity bits >= 0
  where disparity = sum . map (\bit -> if bit then 1 else -1)

toNumber :: [Bool] -> Integer
toNumber = sum . zipWith bitValue [0..] . reverse
  where bitValue pos bit = if bit then 2^pos else 0
