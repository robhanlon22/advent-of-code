-- |
module Day3 where

type Coordinate = (Int, Int)

type Map = [String]

data Cell = Open | Tree deriving (Enum)

paths :: [Coordinate]
paths =
  [ (1, 1),
    (1, 3),
    (1, 5),
    (1, 7),
    (2, 1)
  ]

cell :: Char -> Int
cell '#' = 1
cell _ = 0

cellAt :: Map -> Coordinate -> Int
cellAt rows (r, c) = cell ((rows !! r) !! c)

search :: Map -> Coordinate -> Coordinate -> Int -> Int
search rows (dr, dc) (r, c) s
  | r < length rows =
    search rows (dr, dc) (r + dr, c + dc) (s + cellAt rows (r, c))
  | otherwise = s

descend :: Map -> Coordinate -> Int
descend rows path =
  search rows path (0, 0) 0

pathProduct :: String -> Int
pathProduct content =
  let rows = map cycle $ lines content
   in product $ map (descend rows) paths

part1 :: IO Int
part1 = pathProduct <$> readFile "../input/day3-actual.txt"
