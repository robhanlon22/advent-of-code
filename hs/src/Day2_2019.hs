{-# LANGUAGE FlexibleContexts #-}

-- |
module Day2_2019 where

import Control.Concurrent.Chan (newChan)
import Control.Monad (filterM)
import Data.HashMap.Strict (insert)
import Intcode (ExecutionEvent, Memory, Program (..), eval, extract0, readMemory)

sample :: IO String
sample = return "1,9,10,3,2,3,11,0,99,30,40,50"

actual :: IO String
actual = readFile "../input/day2-2019-actual.txt"

pairs :: [(Integer, Integer)]
pairs = [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]

patch :: (Integer, Integer) -> Memory -> Memory
patch (noun, verb) = insert 1 noun . insert 2 verb

correctResult :: Integer
correctResult = 19690720

isCorrectResult :: (ExecutionEvent, [Program]) -> Bool
isCorrectResult result = case extract0 result of
  Just x -> x == correctResult
  _ -> False

findPair ::
  Memory ->
  (Integer, Integer) ->
  IO Bool
findPair memory pair = do
  input <- newChan
  output <- newChan
  isCorrectResult <$> eval input output (patch pair memory)

part2' :: String -> IO (Maybe Integer)
part2' source = case readMemory source of
  Right memory -> do
    x <- filterM (findPair memory) pairs
    return $ case x of
      [] -> Nothing
      ((noun, verb) : _) -> Just $ (noun * 100) + verb
  Left _ -> return Nothing

part2 :: IO String -> IO (Maybe Integer)
part2 input = do
  x <- input
  part2' x
