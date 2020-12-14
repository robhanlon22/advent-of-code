{-# LANGUAGE MultiWayIf #-}

module Day11 where

import Control.Monad (foldM)
import Data.HashMap.Strict (HashMap, elems, empty, insert, lookup, mapWithKey)
import Prelude hiding (lookup)

actual :: IO String
actual = readFile "../input/day11-actual.txt"

data Cell = Occupied | Unoccupied | Floor deriving (Eq, Show)

type Point = (Int, Int)

type Grid = HashMap Point Cell

deltas :: [Point]
deltas =
  [ (i, j) | i <- [-1 .. 1], j <- [-1 .. 1], not (i == 0 && j == 0)
  ]

sumPoints :: Point -> Point -> Point
sumPoints (i, j) (k, l) = (i + k, j + l)

rays :: [[Point]]
rays = map f deltas
  where
    f x = scanl sumPoints x (repeat x)

fromChar :: Char -> Cell
fromChar '#' = Occupied
fromChar 'L' = Unoccupied
fromChar '.' = Floor

parse :: String -> Grid
parse s =
  foldl
    (\acc p@(i, j) -> insert p (fromChar ((raw !! i) !! j)) acc)
    empty
    points
  where
    raw = lines s
    points =
      [(i, j) | i <- [0 .. pred (length raw)], j <- [0 .. pred (length (raw !! i))]]

visible :: Point -> Grid -> [Cell]
visible point grid =
  foldl
    ( \a ray ->
        either id id $
          foldM
            ( \b delta ->
                case lookup (sumPoints point delta) grid of
                  Just Floor -> Right b
                  Just x -> Left (x : b)
                  Nothing -> Left b
            )
            a
            ray
    )
    []
    rays

countOccupied :: [Cell] -> Int
countOccupied = length . filter (== Occupied)

occupiedVisible :: Point -> Grid -> Int
occupiedVisible point grid = countOccupied $ visible point grid

nextGrid :: Grid -> Grid
nextGrid grid =
  mapWithKey
    ( \point seat ->
        if seat == Floor
          then seat
          else
            let v = occupiedVisible point grid
             in if
                    | (seat == Unoccupied) && (v == 0) -> Occupied
                    | (seat == Occupied) && (v >= 5) -> Unoccupied
                    | otherwise -> seat
    )
    grid

solve :: Grid -> Grid
solve grid =
  if grid == grid'
    then grid'
    else solve grid'
  where
    grid' = nextGrid grid

solution :: String -> Int
solution = countOccupied . elems . solve . parse

main :: IO Int
main = solution <$> actual
