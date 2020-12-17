{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day13_2019 where

import Control.Concurrent
  ( forkFinally,
    newEmptyMVar,
    putMVar,
    tryTakeMVar,
  )
import Control.Concurrent.MVar
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List
import Debug.Trace
import Intcode (readEval)

actual = readFile "../input/day-13.txt"

patched = readFile "../input/day-13-patched.txt"

type Position = (Integer, Integer)

data State = State
  { blockPositions :: S.HashSet Position,
    ballPosition :: Position,
    grid :: M.HashMap Position Integer,
    score :: Integer,
    joystick :: Integer,
    paddle :: Integer
  }
  deriving (Eq)

instance Show State where
  show State {grid, score, blockPositions, ballPosition} =
    let elems = M.keys grid
        ys = map snd elems
        minY = minimum ys
        maxY = maximum ys
        xs = map fst elems
        minX = minimum xs
        maxX = maximum xs
     in "score: "
          ++ show score
          ++ "\n"
          ++ "block positions: "
          ++ show blockPositions
          ++ "\n"
          ++ "ball position: "
          ++ show ballPosition
          ++ "\n"
          ++ unlines
            ( map
                ( \y ->
                    intercalate
                      ""
                      ( map
                          ( \x ->
                              ( case M.lookup (x, y) grid of
                                  Nothing -> " "
                                  Just 0 -> " "
                                  Just 1 -> "■"
                                  Just 2 -> "□"
                                  Just 3 -> "-"
                                  Just 4 -> "●"
                              )
                          )
                          [minX .. maxX]
                      )
                )
                [minY .. maxY]
            )

action (x, y) value state =
  case value of
    0 ->
      state'
        { blockPositions =
            S.delete (x, y) (blockPositions state')
        }
    2 ->
      state'
        { blockPositions =
            S.insert (x, y) (blockPositions state')
        }
    3 -> state' {paddle = x}
    4 ->
      let joystick =
            if
                | x < paddle state -> -1
                | x > paddle state -> 1
                | otherwise -> 0
       in state
            { joystick = joystick,
              ballPosition = (x, y)
            }
    _ -> state'
  where
    state' =
      state
        { grid =
            M.insert (x, y) value (grid state)
        }

play input output done =
  p
    State
      { blockPositions = S.empty,
        ballPosition = (-1, -1),
        grid = M.empty,
        joystick = 0,
        paddle = -1,
        score = -1
      }
  where
    p state = do
      -- d <- tryTakeMVar done
      -- case d of
      --   Just _ -> return state
      --   Nothing -> do
      x <- atomically $ readTQueue input
      y <- atomically $ readTQueue input
      value <- atomically $ readTQueue input
      case (x, y) of
        (-1, 0) -> do
          atomically $ writeTQueue output (joystick state)
          p state {score = value}
        _ -> do
          let state' = action (x, y) value state
           in if score state' > 0 && null (blockPositions state')
                then return state'
                else do
                  atomically $ writeTQueue output (joystick state')
                  p state'

part1' source = do
  input <- newTQueueIO
  output <- newTQueueIO

  done <- newEmptyMVar

  forkFinally
    (readEval input output source)
    (\_ -> putMVar done ())

  play output input done

part1 input = do
  x <- input
  part1' x
