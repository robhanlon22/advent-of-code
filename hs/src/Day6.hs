{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import qualified Data.HashSet as H
import Data.Text (Text, pack, splitOn, unpack)
import Debug.Trace

inter :: [Text] -> Int
inter = length . foldl1 H.intersection . map (H.fromList . unpack)

part2' :: String -> Int
part2' = trace ("aaa") (sum . map (inter . splitOn "\n") . splitOn "\n\n" . pack)

part2 :: IO Int
part2 = do
  contents <- readFile "../input/day6-actual.txt"
  return (part2' contents)
