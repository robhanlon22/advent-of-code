-- |
module Day7_2019 where

import Control.Concurrent
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Monad (foldM, replicateM)
import Data.List (permutations)
import Debug.Trace
import Intcode (readEval)

actual :: IO String
actual = readFile "../input/day7-2019.txt"

codes :: [[Integer]]
codes = permutations [0 .. 4]

codes2 = permutations [5 .. 9]

rotate = drop <> take

part1' source = do
  maximum
    <$> mapM
      ( foldM
          ( \b e -> do
              input <- newTQueueIO
              output <- newTQueueIO
              atomically $ writeTQueue input e
              atomically $ writeTQueue input b
              readEval input output source
              atomically $ readTQueue output
          )
          0
      )
      codes

part1 input = do
  x <- input
  part1' x

part2' source = do
  maximum
    <$> mapM
      ( \code -> do
          inputs <- replicateM (length code) newTQueueIO
          let outputs = rotate 1 inputs

          mvars <-
            mapM
              ( \(setting, input, output) -> do
                  mvar <- newEmptyMVar
                  atomically $ writeTQueue input setting
                  forkFinally
                    (readEval input output source)
                    (\_ -> putMVar mvar $ atomically $ readTQueue output)
                  return mvar
              )
              (zip3 code inputs outputs)

          atomically $ writeTQueue (head inputs) 0

          results <- mapM takeMVar mvars
          last results
      )
      codes2

part2 input = do
  x <- input
  part2' x
