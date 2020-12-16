-- |
module Day9_2019 where

import Control.Concurrent.STM (atomically, flushTQueue, newTQueueIO, writeTQueue)
import Intcode (readEval)

actual :: IO String
actual = readFile "../input/day9-2019.txt"

part1' source = do
  input <- newTQueueIO
  output <- newTQueueIO
  atomically $ writeTQueue input 1
  readEval input output source
  atomically $ flushTQueue output

part1 input = do
  x <- input
  part1' x

part2' source = do
  input <- newTQueueIO
  output <- newTQueueIO
  atomically $ writeTQueue input 2
  readEval input output source
  atomically $ flushTQueue output

part2 input = do
  x <- input
  part2' x

sample2 = do
  input <- newTQueueIO
  output <- newTQueueIO
  readEval input output "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
  out <- atomically (flushTQueue output)
  print out

  input <- newTQueueIO
  output <- newTQueueIO
  readEval input output "1102,34915192,34915192,7,4,7,99,0"
  out <- atomically (flushTQueue output)
  print out

  input <- newTQueueIO
  output <- newTQueueIO
  readEval input output "104,1125899906842624,99"
  out <- atomically (flushTQueue output)
  print out
