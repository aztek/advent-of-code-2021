main :: IO ()
main = do
  input <- parse <$> getContents
  print (part1 input)
  print (part2 input)

part1 :: Input -> Int
part1 (algo, image) = length (filter id (concat pixels))
  where (_, pixels) = enhanceMore algo 2 image

part2 :: Input -> Int
part2 (algo, image) = length (filter id (concat pixels))
  where (_, pixels) = enhanceMore algo 50 image

type Pixel = Bool
type Algo = [Pixel]
type Image = (Pixel, [[Pixel]])
type Input = (Algo, Image)

parse :: String -> Input
parse str = (map (== '#') algo, (False, map (map (== '#')) image))
  where (algo:[]:image) = lines str

enhanceMore :: Algo -> Int -> Image -> Image
enhanceMore algo n image = iterate (enhanceImage algo) image !! n

enhanceImage :: Algo -> Image -> Image
enhanceImage algo (pix, image) = (pix', image')
  where (height, width) = dimentions image
        pix' = enhance algo (replicate 8 pix)
        image' = [[enhancePixel (x, y) | y <- [-1..width]] | x <- [-1..height]]
        enhancePixel = enhance algo . map pixelAt . adjacent
        pixelAt (x, y)
          | x >= 0 && x < height && y >= 0 && y < width = image !! x !! y
          | otherwise = pix

dimentions :: [[a]] -> (Int, Int)
dimentions matrix = (length matrix, length (head matrix))

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = do dx <- [-1..1]
                     dy <- [-1..1]
                     return (x + dx, y + dy)

enhance :: Algo -> [Pixel] -> Pixel
enhance algo neightbours = algo !! fromBitvector neightbours

fromBitvector :: [Bool] -> Int
fromBitvector = sum . zipWith bitValue [0..] . reverse
  where bitValue pos bit = if bit then 2^pos else 0
