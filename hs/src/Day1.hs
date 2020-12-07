module Day1
  ( expenseReport,
    part1,
  )
where

import Data.List (find)
import Math.Combinat.Sets (choose)

expenseReport =
  [ 1721,
    979,
    366,
    299,
    675,
    1456
  ]

sumsTo2020 :: [[Integer]] -> Maybe [Integer]
sumsTo2020 = find $ (== 2020) . sum

part1 :: [Integer] -> Int -> Maybe Integer
part1 input n = fmap product $ sumsTo2020 $ choose n input
