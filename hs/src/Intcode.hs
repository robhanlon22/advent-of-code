{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Intcode where

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTQueue,
    readTVarIO,
    writeTQueue,
  )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader
  ( MonadReader (ask, local),
    ReaderT (runReaderT),
    asks,
    void,
  )
import Data.HashMap.Strict (HashMap, findWithDefault, fromList, insert, (!?))
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
  ( ParseErrorBundle,
    Parsec,
    empty,
    eof,
    many,
    optional,
    runParser,
  )
import Text.Megaparsec.Char (char, newline, spaceChar)
import Text.Megaparsec.Char.Lexer (decimal, lexeme, signed, space)

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

signedInteger =
  let spaceConsumer = space (void spaceChar) empty empty
   in signed spaceConsumer $ lexeme spaceConsumer decimal

pRawMemory =
  ( do
      elements <- many $ do
        element <- signedInteger
        optional $ void (char ',')
        return element
      optional $ void newline
      return elements
  )
    <* eof

mkMemory = fromList . zip [0 ..]

readMemory source = mkMemory <$> runParser pRawMemory "" (pack source)

data Status
  = Halted
  | BadInstruction
  | BadPointer
  | BadMode
  deriving (Eq, Show)

data ExecutionEvent = ExecutionEvent Status Integer
  deriving (Eq, Show)

type Memory = HashMap Integer Integer

data System = System
  { program :: Program,
    journal :: TVar [Program]
  }

instance Show System where
  show System {program} = show program

data Program = Program
  { input :: TQueue Integer,
    output :: TQueue Integer,
    pointer :: Integer,
    memory :: Memory
  }
  deriving (Eq)

instance Show Program where
  show Program {pointer, memory} = show (pointer, memory)

type Result = Either ExecutionEvent Program

data Mode
  = Position
  | Immediate
  | Relative
  deriving (Eq, Show)

data Opcode
  = Add
  | Multiply
  | Input
  | Output
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | AdjustRelativeBase
  | Halt
  deriving (Eq, Show)

data Instruction = Instruction
  { opcode :: Opcode,
    mode1 :: Mode,
    mode2 :: Mode,
    mode3 :: Mode,
    mode4 :: Mode
  }
  deriving (Eq, Show)

readAddr pointer memory =
  if pointer < 0
    then Left $ ExecutionEvent BadPointer pointer
    else Right $ findWithDefault 0 pointer memory

readVal mode pointer memory = do
  x <- readAddr pointer memory
  if mode == Position
    then readAddr x memory
    else return x

binaryOp ::
  MonadReader System m =>
  Instruction ->
  ( Integer ->
    Integer ->
    Integer
  ) ->
  m Result
binaryOp Instruction {mode1, mode2} f = do
  program@Program {pointer, memory} <- asks program
  return $ do
    lhs <- readVal mode1 (pointer + 1) memory
    rhs <- readVal mode2 (pointer + 2) memory
    dest <- readAddr (pointer + 3) memory
    return
      program
        { pointer = pointer + 4,
          memory = insert dest (f lhs rhs) memory
        }

conditionalOp ::
  MonadReader System m =>
  Instruction ->
  ( Integer ->
    Integer ->
    Bool
  ) ->
  m Result
conditionalOp instruction f = do
  binaryOp
    instruction
    ( \lhs rhs ->
        if f lhs rhs
          then 1
          else 0
    )

jumpOp ::
  MonadReader System m =>
  Instruction ->
  Bool ->
  m Result
jumpOp Instruction {mode1, mode2} b = do
  program@Program {pointer, memory} <- asks program
  return $ do
    case readVal mode1 (pointer + 1) memory of
      Right lhs -> do
        case if (lhs == 0) == b
          then Right $ pointer + 3
          else readVal mode2 (pointer + 2) memory of
          Right pointer' -> Right program {pointer = pointer'}
          Left event -> Left event
      Left event -> Left event

exec ::
  ( MonadIO m,
    MonadReader System m
  ) =>
  Instruction ->
  m Result
exec instruction@Instruction {opcode = Add} = binaryOp instruction (+)
exec instruction@Instruction {opcode = Multiply} = binaryOp instruction (*)
exec Instruction {opcode = Input} = do
  program@Program {input, pointer, memory} <- asks program
  value <- liftIO $ atomically $ readTQueue input
  return $ do
    dest <- readAddr (pointer + 1) memory
    let pointer' = pointer + 2
    return
      program
        { memory = insert dest value memory,
          pointer = pointer'
        }
exec Instruction {opcode = Output, mode1} = do
  program@Program {output, pointer, memory} <- asks program
  case readVal mode1 (pointer + 1) memory of
    Right value -> do
      liftIO $ atomically $ writeTQueue output value
      return $ Right program {pointer = pointer + 2}
    Left event -> return $ Left event
exec instruction@Instruction {opcode = JumpIfTrue} =
  jumpOp instruction True
exec instruction@Instruction {opcode = JumpIfFalse} =
  jumpOp instruction False
exec instruction@Instruction {opcode = LessThan} =
  conditionalOp instruction (<)
exec instruction@Instruction {opcode = Equals} =
  conditionalOp instruction (==)
exec Instruction {opcode = AdjustRelativeBase} = do
  program@Program {output, pointer, memory} <- asks program
  return $ Right program
exec Instruction {opcode = Halt} = do
  Program {pointer} <- asks program
  return $ Left $ ExecutionEvent Halted pointer

mkOpcode :: Integer -> Either ExecutionEvent Opcode
mkOpcode 1 = Right Add
mkOpcode 2 = Right Multiply
mkOpcode 3 = Right Input
mkOpcode 4 = Right Output
mkOpcode 5 = Right JumpIfTrue
mkOpcode 6 = Right JumpIfFalse
mkOpcode 7 = Right LessThan
mkOpcode 8 = Right Equals
mkOpcode 9 = Right AdjustRelativeBase
mkOpcode 99 = Right Halt
mkOpcode x = Left $ ExecutionEvent BadInstruction x

mkMode :: Integer -> Either ExecutionEvent Mode
mkMode 0 = Right Position
mkMode 1 = Right Immediate
mkMode 2 = Right Relative
mkMode x = Left $ ExecutionEvent BadMode x

mkInstruction :: Integer -> Either ExecutionEvent Instruction
mkInstruction value = do
  opcode <- mkOpcode $ value `mod` 100
  mode1 <- mkMode $ (value `div` 100) `mod` 10
  mode2 <- mkMode $ (value `div` 1000) `mod` 10
  mode3 <- mkMode $ (value `div` 10000) `mod` 10
  mode4 <- mkMode $ (value `div` 100000) `mod` 10
  return
    Instruction
      { opcode = opcode,
        mode1 = mode1,
        mode2 = mode2,
        mode3 = mode3,
        mode4 = mode4
      }

step :: (MonadIO m, MonadReader System m) => m Result
step = do
  Program {pointer, memory} <- asks program
  case readAddr pointer memory of
    Right opcode -> do
      case mkInstruction opcode of
        Right instruction -> exec instruction
        Left event -> return $ Left event
    Left event -> return $ Left event

doJournal :: (MonadIO m, MonadReader System m) => m ()
doJournal = do
  p <- asks program
  j <- asks journal
  liftIO $
    atomically $
      modifyTVar' j (p :)

runLoop :: (MonadIO m, MonadReader System m) => m ExecutionEvent
runLoop = do
  doJournal
  result <- step
  case result of
    Right p -> local (\system -> system {program = p}) runLoop
    Left e -> return e

eval ::
  TQueue Integer ->
  TQueue Integer ->
  Memory ->
  IO (ExecutionEvent, [Program])
eval input output mem = do
  system <- mkSystem input output mem
  event <- runReaderT runLoop system
  journal <- liftIO $ readTVarIO (journal system)
  return (event, journal)

mkSystem ::
  (MonadIO m) =>
  TQueue Integer ->
  TQueue Integer ->
  Memory ->
  m System
mkSystem input output memory = do
  journal <- liftIO $ newTVarIO []
  return $
    System
      { journal = journal,
        program =
          Program
            { input = input,
              output = output,
              pointer = 0,
              memory = memory
            }
      }

readEval ::
  TQueue Integer ->
  TQueue Integer ->
  String ->
  IO (Either ParserError (ExecutionEvent, [Program]))
readEval input output source = do
  either
    (return . Left)
    (fmap Right . eval input output)
    (readMemory source)

extract0 :: (ExecutionEvent, [Program]) -> Maybe Integer
extract0 (ExecutionEvent Halted _, Program {memory} : _) = memory !? 0
extract0 _ = Nothing
