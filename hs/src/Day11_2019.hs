{-# LANGUAGE NamedFieldPuns #-}

-- |
module Day11_2019 where

import Control.Concurrent
  ( forkFinally,
    newEmptyMVar,
    putMVar,
    tryTakeMVar,
  )
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (intercalate)
import Intcode (readEval)

actual = readFile "../input/day11-2019.txt"

type Point = (Integer, Integer)

data State = State
  { position :: Point,
    grid :: M.HashMap Point Integer,
    visited :: S.HashSet Point,
    delta :: Point
  }
  deriving (Eq)

instance Show State where
  show State {grid} =
    let elems = M.keys grid
        ys = map snd elems
        minY = minimum ys
        maxY = maximum ys
        xs = map fst elems
        minX = minimum xs
        maxX = maximum xs
     in unlines
          ( map
              ( \y ->
                  intercalate
                    ""
                    ( map
                        ( \x ->
                            ( case M.lookup (x, y) grid of
                                Nothing -> " "
                                Just 0 -> "."
                                Just 1 -> "#"
                            )
                        )
                        [minX .. maxX]
                    )
              )
              [minY .. maxY]
          )

update
  State
    { position = position@(x, y),
      grid,
      visited,
      delta = (dx, dy)
    }
  color
  direction =
    State
      { position = (x + dx', y - dy'),
        grid = M.insert position color grid,
        visited = S.insert position visited,
        delta = (dx', dy')
      }
    where
      (dx', dy') =
        if direction == 0
          then (- dy, dx)
          else (dy, - dx)

paint input output done =
  p
    State
      { position = (0, 0),
        grid = M.singleton (0, 0) 1,
        visited = S.empty,
        delta = (0, 1)
      }
  where
    p state@State {position, grid} = do
      d <- tryTakeMVar done
      case d of
        Just _ -> print state
        Nothing -> do
          atomically $ writeTQueue output $ M.findWithDefault 0 position grid
          color <- atomically $ readTQueue input
          direction <- atomically $ readTQueue input
          p $ update state color direction

part1' source = do
  input <- newTQueueIO
  output <- newTQueueIO

  done <- newEmptyMVar

  forkFinally
    (readEval input output source)
    (\_ -> putMVar done ())

  paint output input done

part1 input = do
  x <- input
  part1' x
