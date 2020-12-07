-- |
module Day5 where

bintodec :: [Bool] -> Int
bintodec = foldl (\x y -> fromEnum y + 2 * x) 0

passCharToBool :: Char -> Bool
passCharToBool 'F' = False
passCharToBool 'B' = True
passCharToBool 'L' = False
passCharToBool 'R' = True

passToBools :: String -> [Bool]
passToBools = reverse . map passCharToBool

maxPassId :: String -> Int
maxPassId = maximum . map (bintodec . passToBools) . lines

part1 :: IO Int
part1 = maxPassId <$> readFile "../input/day5-actual.txt"
