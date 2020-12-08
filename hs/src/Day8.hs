{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Control.Monad
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import Debug.Trace

data Opcode = Acc | Jmp | Nop deriving (Show, Eq)

type Instruction = (Opcode, Int)

data Regs = Regs
  { acc :: Int,
    ptr :: Int
  }

type Program = S.Seq Instruction

data Status = Infinite | Done | Invalid deriving (Show, Eq)

data Result = Result Status Int deriving (Show, Eq)

parse :: [String] -> Instruction
parse (op : (s : n) : _) =
  let opcode = case op of
        "nop" -> Nop
        "acc" -> Acc
        "jmp" -> Jmp
      m = case s of
        '-' -> -1
        '+' -> 1
   in (opcode, m * read n)

call :: Instruction -> Regs -> Regs
call (Nop, _) regs@Regs {ptr} = regs {ptr = succ ptr}
call (Acc, v) regs@Regs {acc, ptr} = regs {acc = acc + v, ptr = succ ptr}
call (Jmp, v) regs@Regs {ptr} = regs {ptr = ptr + v}

readProg :: String -> Program
readProg = S.fromList . map (parse . words) . lines

execProg' :: HS.HashSet Int -> Regs -> Program -> Result
execProg' visited regs@Regs {acc, ptr} p
  | HS.member ptr visited = Result Infinite acc
  | ptr == length p = Result Done acc
  | ptr > length p = Result Invalid acc
  | otherwise =
    let visited' = HS.insert ptr visited
        instruction = S.index p ptr
        regs' = call instruction regs
     in execProg' visited' regs' p

execProg :: Program -> Result
execProg = execProg' HS.empty Regs {acc = 0, ptr = 0}

part1' :: String -> Maybe Result
part1' = Just . execProg . readProg

execPermutation :: Program -> Int -> Int -> Either Result Int
execPermutation p _ i =
  case r of
    (Result Done _) -> Left r
    _ -> Right i
  where
    (op, v) = S.index p i
    op' =
      case op of
        Nop -> Jmp
        Jmp -> Nop
        Acc -> Acc
    p' = S.update i (op', v) p
    r = execProg p'

findMutation :: Program -> Maybe Result
findMutation p =
  either Just (const Nothing) $
    foldM (execPermutation p) 0 [0 .. (pred (length p))]

part2' :: String -> Maybe Result
part2' = findMutation . readProg

part1 :: String -> IO (Maybe Result)
part1 f = part1' <$> readFile f

part2 :: String -> IO (Maybe Result)
part2 f = part2' <$> readFile f
