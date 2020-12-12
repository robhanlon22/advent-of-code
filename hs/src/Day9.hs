-- |
module Day9 where

import Data.List
import Data.List.Split
import Data.Maybe

combinations :: Int -> [a] -> [[a]]
combinations = (. subsequences) . filter . (. length) . (==)

firstInvalid :: Int -> [Int] -> Int
firstInvalid len xmas = f ps
  where
    ps = divvy (succ len) 1 xmas
    f (p : ps) =
      let n = last p
       in if elem n $ map sum $ combinations 2 $ take len p
            then f ps
            else n

readXmas :: String -> [Int]
readXmas = map read . lines

part1' :: Int -> String -> Int
part1' len s = firstInvalid len $ readXmas s

part1 :: Int -> String -> IO Int
part1 len f = part1' len <$> readFile f

part2' :: Int -> String -> Int
part2' len s = minimum x + maximum x
  where
    xmas = readXmas s
    n = firstInvalid len xmas
    i = pred $ fromJust $ elemIndex n xmas
    p = concatMap (\x -> divvy x 1 xmas) [2 .. i]
    x = fromJust $ find (\x -> sum x == n) p

part2 :: Int -> String -> IO Int
part2 len f = part2' len <$> readFile f
