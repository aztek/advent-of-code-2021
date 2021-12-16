{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (first, second, bimap)
import Data.Char (digitToInt)

main :: IO ()
main = do
  packet <- parse <$> getContents
  -- Part 1
  print $ sum (versions packet)
  -- Part 2
  print (eval packet)


-- * Bitvectors

type Bit = Bool
type Bitvector = [Bit]

fromBitvector :: Bitvector -> Int
fromBitvector = sum . zipWith bitValue [0..] . reverse
  where bitValue pos bit = if bit then 2^pos else 0

parse :: String -> Packet
parse = parseTransmission . concatMap (parseHex . digitToInt)
  where parseHex n = [div n 8 == 1,
                      div (rem n 8) 4 == 1,
                      div (rem n 4) 2 == 1,
                      div (rem n 2) 1 == 1]


-- * Packets

type Version = Int

data Packet
  = Literal Version Int
  | Operator Version Operation [Packet]
  deriving (Eq, Show)

data Operation
  = Sum
  | Product
  | Minimum
  | Maximum
  | GreaterThen
  | LessThen
  | Equals
  deriving (Eq, Show, Enum)

versions :: Packet -> [Int]
versions (Literal ver _) = [ver]
versions (Operator ver _ ps) = ver : concatMap versions ps

eval :: Packet -> Int
eval = \case
  Literal _ val -> val
  Operator _ op ps -> evalOperation op (map eval ps)

evalOperation :: Operation -> [Int] -> Int
evalOperation = \case
  Sum         -> sum
  Product     -> product
  Minimum     -> minimum
  Maximum     -> maximum
  GreaterThen -> \[a, b] -> if a > b then 1 else 0
  LessThen    -> \[a, b] -> if a < b then 1 else 0
  Equals      -> \[a, b] -> if a == b then 1 else 0


-- * Parsing

parseTransmission :: Bitvector -> Packet
parseTransmission bv = packet where (packet, _padding) = parsePacket bv

type Parser a = [Bit] -> (a, [Bit])

parsePacket :: Parser Packet
parsePacket packet =
  case parseNumber 3 payload of
    (4, bv) -> first (Literal ver) (parseLiteral bv)
    (o, bv) -> first (Operator ver (parseOperation o)) (parseOperator bv)
  where (ver, payload) = parseNumber 3 packet

parseOperation :: Int -> Operation
parseOperation = \case
  0 -> Sum
  1 -> Product
  2 -> Minimum
  3 -> Maximum
  5 -> GreaterThen
  6 -> LessThen
  7 -> Equals

parseNumber :: Int -> Parser Int
parseNumber size = first fromBitvector . splitAt size

parseBool :: Parser Bool
parseBool bv = (head bv, tail bv)

parseLiteral :: Parser Int
parseLiteral = helper True []
  where helper False acc bv = (fromBitvector acc, bv)
        helper True  acc bv = helper d (acc ++ ds) bv'
          where (d:ds, bv') = splitAt 5 bv

parseOperator :: Parser [Packet]
parseOperator = uncurry lengthType . parseBool
  where lengthType True  = uncurry (parseTimes parsePacket) . parseNumber 11
        lengthType False = uncurry (parseSize parsePacket) . parseNumber 15

parseSize :: Parser a -> Int -> Parser [a]
parseSize p = helper []
  where helper acc 0 bv = (reverse acc, bv)
        helper acc n bv = helper (x:acc) (n - size) bv'
          where (x, bv') = p bv
                size = length bv - length bv'

parseTimes :: Parser a -> Int -> Parser [a]
parseTimes p = helper []
  where helper acc 0 bv = (reverse acc, bv)
        helper acc n bv = helper (x:acc) (n-1) bv'
          where (x, bv') = p bv
