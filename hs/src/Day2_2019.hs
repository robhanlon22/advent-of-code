-- |
module Day2_2019 where

import qualified Data.HashMap.Strict as HM
import Data.List
import Intcode

sample :: IO String
sample = return "1,9,10,3,2,3,11,0,99,30,40,50"

actual :: IO String
actual = readFile "../input/day2-2019-actual.txt"

readEvalPatched source =
  eval
    . HM.insert 2 2
    . HM.insert 1 12
    <$> readMemory source

solvePatched :: String -> Either ParserError (Maybe Int)
solvePatched source = extractSolution <$> readEvalPatched source

part1 input = solvePatched <$> input

pairs = [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]

part2' :: String -> Maybe Int
part2' source =
  ( \(noun, verb) ->
      (100 * noun) + verb
  )
    <$> find
      ( \(noun, verb) ->
          case extractSolution
            . eval
            . HM.insert 1 noun
            . HM.insert 2 verb
            <$> readMemory source of
            Right (Just x) -> x == 19690720
            _ -> False
      )
      pairs

part2 :: IO String -> IO (Maybe Int)
part2 input = part2' <$> input
