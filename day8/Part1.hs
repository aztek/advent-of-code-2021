main :: IO ()
main = do
  entries <- fmap parseEntry . lines <$> getContents
  print (countSomeDigits (map snd entries))

parseEntry :: String -> ([String], [String])
parseEntry s = (inputs, tail outputs)
  where (inputs, outputs) = span (/= "|") (words s)

countSomeDigits :: [[String]] -> Int
countSomeDigits = length . filter isSomeDigit . concat

isSomeDigit :: String -> Bool
isSomeDigit word = length word `elem` [2, 3, 4, 7]
