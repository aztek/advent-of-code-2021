import Data.Char (ord)
import Data.List (find, sort, elemIndex, permutations)
import Data.Maybe (isJust, fromJust)

main :: IO ()
main = do
  entries <- fmap parseEntry . lines <$> getContents
  print (sum (map (fromJust . uncurry solveEntry) entries))

parseEntry :: String -> ([String], [String])
parseEntry s = (inputs, tail outputs)
  where (inputs, outputs) = span (/= "|") (words s)

solveEntry :: [String] -> [String] -> Maybe Int
solveEntry input digits = do
  sol <- solve input
  decodings <- mapM (decode sol) digits
  let juxtapose = read . concatMap show
  return (juxtapose decodings)

type Solution = String

solve :: [String] -> Maybe Solution
solve digits = find isSolution (permutations ['a'..'g'])
  where isSolution perm = all (isJust . decode perm) digits

decode :: Solution -> String -> Maybe Int
decode sol signal = elemIndex (sort decoding) validSignals
  where decoding = [sol !! (ord d - ord 'a') | d <- signal]
        validSignals = ["abcefg", "cf", "acdeg", "acdfg", "bcdf",
                        "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]
