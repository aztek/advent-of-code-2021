-- cat input1.txt | runhaskell Part1.hs

import Data.List (transpose)
import Data.Bifunctor (bimap)

main :: IO ()
main = do
  let parseDiagnostic = map (== '1')
  report <- fmap parseDiagnostic . lines <$> getContents
  let (gamma, epsilon) = rates report
  print (gamma * epsilon)

rates :: [[Bool]] -> (Integer, Integer)
rates = bimap toNumber toNumber . bitRates
  where bitRates = unzip . map (bits . frequentBit) . transpose
        bits gamma = (gamma, not gamma)

frequentBit :: [Bool] -> Bool
frequentBit bits = disparity bits >= 0
  where disparity = sum . map (\bit -> if bit then 1 else -1)

toNumber :: [Bool] -> Integer
toNumber = sum . zipWith bitValue [0..] . reverse
  where bitValue pos bit = if bit then 2^pos else 0
