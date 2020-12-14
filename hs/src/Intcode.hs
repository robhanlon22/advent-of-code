{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Intcode where

import Control.Concurrent.Chan
import Control.Monad.Trans
import Control.Monad.Writer
  ( MonadWriter (writer),
    Writer,
    fix,
    runWriterT,
    tell,
    void,
  )
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, eof, many, optional, runParser)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

pRawMemory =
  ( do
      elements <- many $ do
        element <- decimal
        optional $ void (char ',')
        return element
      optional $ void newline
      return elements
  )
    <* eof

mkMemory = HM.fromList . zip [0 ..]

readMemory source = mkMemory <$> runParser pRawMemory "" (pack source)

data ExecutionEvent
  = Halt
  | BadPointer
  | BadInstruction
  deriving (Eq, Show)

type Memory = HM.HashMap Integer Integer

data System = System
  { pointer :: Integer,
    memory :: Memory,
    input :: IO (Chan Integer),
    output :: IO (Chan Integer)
  }

instance Show System where
  show System {pointer, memory} = show (pointer, memory)

type Result = Either ExecutionEvent System

exec ::
  System -> Maybe Integer -> IO Result
exec system@System {pointer, memory} (Just 1) =
  return $ case x of
    Just s -> Right s
    Nothing -> Left BadInstruction
  where
    x = do
      lsrc <- HM.lookup (pointer + 1) memory
      rsrc <- HM.lookup (pointer + 2) memory
      dest <- HM.lookup (pointer + 3) memory
      lhs <- HM.lookup lsrc memory
      rhs <- HM.lookup rsrc memory
      let memory' = HM.insert dest (lhs + rhs) memory
      return system {pointer = pointer + 4, memory = memory'}
exec system@System {pointer, memory} (Just 2) =
  return $ case x of
    Just s -> Right s
    Nothing -> Left BadInstruction
  where
    x = do
      lsrc <- HM.lookup (pointer + 1) memory
      rsrc <- HM.lookup (pointer + 2) memory
      dest <- HM.lookup (pointer + 3) memory
      lhs <- HM.lookup lsrc memory
      rhs <- HM.lookup rsrc memory
      let memory' = HM.insert dest (lhs * rhs) memory
      return system {pointer = pointer + 4, memory = memory'}
exec system@System {pointer, memory, input} (Just 3) = do
  chan <- input
  value <- readChan chan
  return $ case HM.lookup (pointer + 1) memory of
    Just s ->
      let memory' = HM.insert s value memory
       in Right $
            system
              { pointer = pointer + 2,
                memory = memory'
              }
    Nothing -> Left BadPointer
exec system@System {pointer, memory, output} (Just 4) = do
  chan <- output
  case HM.lookup (pointer + 1) memory of
    Just s -> do
      writeChan chan s
      return $ Right $ system {pointer = pointer + 2}
    Nothing -> return $ Left BadPointer
exec _ (Just 99) = return $ Left Halt
exec _ (Just _) = return $ Left BadInstruction
exec _ Nothing = return $ Left BadPointer

step :: System -> IO Result
step system@System {pointer, memory} =
  exec system (HM.lookup pointer memory)

runLoop ::
  (MonadIO m, MonadWriter [System] m) =>
  System ->
  m Result
runLoop system = do
  system' <- liftIO $ step system
  tell [system]
  case system' of
    Right s -> runLoop s
    event -> return event

eval ::
  MonadIO m =>
  IO (Chan Integer) ->
  IO (Chan Integer) ->
  Memory ->
  m (Result, [System])
eval input output memory = do
  runWriterT $
    runLoop $
      System
        { memory = memory,
          pointer = 0,
          input = input,
          output = output
        }

readEval ::
  MonadIO m =>
  IO (Chan Integer) ->
  IO (Chan Integer) ->
  String ->
  m (Either ParserError (Result, [System]))
readEval input output source = do
  either (return . Left) (fmap Right . eval input output) (readMemory source)

extractSolution :: (Result, [System]) -> Maybe Integer
extractSolution (Left Halt, System {memory} : _) = HM.lookup 0 memory
extractSolution _ = Nothing

solve ::
  MonadIO m =>
  IO (Chan Integer) ->
  IO (Chan Integer) ->
  String ->
  m (Maybe (Either ParserError Integer))
solve input output source = do
  log <- readEval input output source
  return $ either (return . Left) (fmap Right . extractSolution) log
