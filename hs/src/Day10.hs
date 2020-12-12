module Day10 where

import qualified Data.HashSet as HS
import Data.MemoCombinators as Memo

part2' :: String -> Int
part2' s = p 0
  where
    p = Memo.integral p'
    p' x
      | x == m = 1
      | otherwise =
        sum $ map p $ filter (`HS.member` h) $ map (+ x) [1 .. 3]
    m = maximum a
    h = HS.insert 0 $ HS.insert m $ HS.fromList a
    a = map read $ lines s :: [Int]

part2 :: IO Int
part2 = part2' <$> readFile "../input/day10-actual.txt"
