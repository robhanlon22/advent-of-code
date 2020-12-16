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
import Control.Monad.Except (MonadError (throwError), runExceptT, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader
  ( MonadReader (ask, local),
    ReaderT (runReaderT),
    asks,
    void,
  )
import Control.Monad.Trans.Except ()
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
  = BadInstruction
  | BadMode
  | BadPointer
  | Crash
  | Halted
  | UnsupportedMode
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
    relativeBase :: Integer,
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

data Param = Param Mode Integer deriving (Eq, Show)

data Instruction = Instruction
  { opcode :: Opcode,
    param1 :: Param,
    param2 :: Param,
    param3 :: Param,
    param4 :: Param
  }
  deriving (Eq, Show)

readAddr ::
  ( MonadReader System m,
    MonadError ExecutionEvent m
  ) =>
  Integer ->
  m Integer
readAddr pointer = do
  program <- asks program
  if pointer < 0
    then throwError $ ExecutionEvent BadPointer pointer
    else return $ findWithDefault 0 pointer (memory program)

readDest (Param mode pointer) = do
  program <- asks program
  value <- readAddr pointer
  case mode of
    Position -> return value
    Immediate -> throwError $ ExecutionEvent UnsupportedMode pointer
    Relative -> return $ value + relativeBase program

readParam (Param mode pointer) = do
  program <- asks program
  value <- readAddr pointer
  case mode of
    Position -> readAddr value
    Immediate -> return value
    Relative -> readAddr (value + relativeBase program)

binaryOp ::
  ( MonadReader System m,
    MonadError ExecutionEvent m
  ) =>
  Instruction ->
  ( Integer ->
    Integer ->
    Integer
  ) ->
  m Program
binaryOp Instruction {param1, param2, param3} f = do
  program@Program {pointer, memory} <- asks program
  lhs <- readParam param1
  rhs <- readParam param2
  dest <- readDest param3
  return
    program
      { pointer = pointer + 4,
        memory = insert dest (f lhs rhs) memory
      }

conditionalOp ::
  ( MonadReader System m,
    MonadError ExecutionEvent m
  ) =>
  Instruction ->
  ( Integer ->
    Integer ->
    Bool
  ) ->
  m Program
conditionalOp instruction f = do
  binaryOp
    instruction
    ( \lhs rhs ->
        if f lhs rhs
          then 1
          else 0
    )

jumpOp ::
  ( MonadReader System m,
    MonadError ExecutionEvent m
  ) =>
  Instruction ->
  Bool ->
  m Program
jumpOp Instruction {param1, param2} b = do
  program@Program {pointer} <- asks program
  value <- readParam param1
  pointer' <-
    if (value == 0) == b
      then return $ pointer + 3
      else readParam param2
  return program {pointer = pointer'}

exec ::
  ( MonadIO m,
    MonadReader System m,
    MonadError ExecutionEvent m
  ) =>
  Instruction ->
  m Program
exec instruction@Instruction {opcode = Add} = binaryOp instruction (+)
exec instruction@Instruction {opcode = Multiply} = binaryOp instruction (*)
exec Instruction {opcode = Input, param1} = do
  program@Program {input, pointer, memory} <- asks program
  value <- liftIO $ atomically $ readTQueue input
  dest <- readDest param1
  return
    program
      { memory = insert dest value memory,
        pointer = pointer + 2
      }
exec Instruction {opcode = Output, param1} = do
  program@Program {output, pointer} <- asks program
  value <- readParam param1
  liftIO $ atomically $ writeTQueue output value
  return program {pointer = pointer + 2}
exec instruction@Instruction {opcode = JumpIfTrue} =
  jumpOp instruction True
exec instruction@Instruction {opcode = JumpIfFalse} =
  jumpOp instruction False
exec instruction@Instruction {opcode = LessThan} =
  conditionalOp instruction (<)
exec instruction@Instruction {opcode = Equals} =
  conditionalOp instruction (==)
exec Instruction {opcode = AdjustRelativeBase, param1} = do
  program@Program {pointer, relativeBase} <- asks program
  param <- readParam param1
  return
    program
      { relativeBase = relativeBase + param,
        pointer = pointer + 2
      }
exec Instruction {opcode = Halt} = do
  Program {pointer} <- asks program
  throwError $ ExecutionEvent Halted pointer

parseOpcode :: (MonadError ExecutionEvent m) => Integer -> m Opcode
parseOpcode 1 = return Add
parseOpcode 2 = return Multiply
parseOpcode 3 = return Input
parseOpcode 4 = return Output
parseOpcode 5 = return JumpIfTrue
parseOpcode 6 = return JumpIfFalse
parseOpcode 7 = return LessThan
parseOpcode 8 = return Equals
parseOpcode 9 = return AdjustRelativeBase
parseOpcode 99 = return Halt
parseOpcode x = throwError $ ExecutionEvent BadInstruction x

mkParam mode offset = do
  Program {pointer} <- asks program
  return $ Param mode (pointer + offset)

parseParam ::
  ( MonadError ExecutionEvent m,
    MonadReader System m
  ) =>
  Integer ->
  Integer ->
  m Param
parseParam 0 offset = mkParam Position offset
parseParam 1 offset = mkParam Immediate offset
parseParam 2 offset = mkParam Relative offset
parseParam x _ = throwError $ ExecutionEvent BadMode x

parseInstruction ::
  ( MonadError ExecutionEvent m,
    MonadReader System m
  ) =>
  Integer ->
  m Instruction
parseInstruction value = do
  opcode <- parseOpcode $ value `mod` 100
  param1 <- parseParam ((value `div` 100) `mod` 10) 1
  param2 <- parseParam ((value `div` 1000) `mod` 10) 2
  param3 <- parseParam ((value `div` 10000) `mod` 10) 3
  param4 <- parseParam ((value `div` 100000) `mod` 10) 4
  return
    Instruction
      { opcode = opcode,
        param1 = param1,
        param2 = param2,
        param3 = param3,
        param4 = param4
      }

step ::
  ( MonadIO m,
    MonadReader System m,
    MonadError ExecutionEvent m
  ) =>
  m Program
step = do
  Program {pointer} <- asks program
  opcode <- readAddr pointer
  instruction <- parseInstruction opcode
  exec instruction

doJournal :: (MonadIO m, MonadReader System m) => m ()
doJournal = do
  p <- asks program
  j <- asks journal
  liftIO $
    atomically $
      modifyTVar' j (p :)

runLoop ::
  ( MonadIO m,
    MonadReader System m,
    MonadError ExecutionEvent m
  ) =>
  m Program
runLoop = do
  doJournal
  program <- step
  local (\system -> system {program = program}) runLoop

eval ::
  TQueue Integer ->
  TQueue Integer ->
  Memory ->
  IO (ExecutionEvent, [Program])
eval input output mem = do
  system <- mkSystem input output mem
  result <- runExceptT $ runReaderT runLoop system
  journal <- liftIO $ readTVarIO (journal system)
  return
    ( case result of
        Left event -> event
        Right program -> ExecutionEvent Crash (pointer program),
      journal
    )

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
              relativeBase = 0,
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
