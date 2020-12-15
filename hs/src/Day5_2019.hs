-- |
module Day5_2019 where

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    flushTQueue,
    modifyTVar',
    newTQueueIO,
    newTVarIO,
    readTQueue,
    readTVarIO,
    writeTQueue,
  )
import Intcode (readEval)

actual :: IO String
actual = readFile "../input/day5-2019.txt"

part1' source = do
  input <- newTQueueIO
  atomically $ writeTQueue input 1
  output <- newTQueueIO
  readEval input output source
  atomically $ flushTQueue output

part1 input = do
  x <- input
  part1' x

part2' source = do
  input <- newTQueueIO
  atomically $ writeTQueue input 5
  output <- newTQueueIO
  readEval input output source
  atomically $ flushTQueue output

part2 input = do
  x <- input
  part2' x
