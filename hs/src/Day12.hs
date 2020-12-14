{-# LANGUAGE NamedFieldPuns #-}

-- |
module Day12 where

import Control.Applicative (Alternative)
import Control.Monad (void)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, choice, many, parse, (<|>))
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

optional :: Alternative f => f a -> f (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

sample :: IO String
sample = readFile "../input/day12-sample.txt"

actual :: IO String
actual = readFile "../input/day12-actual.txt"

data Action
  = N
  | S
  | E
  | W
  | L
  | R
  | F
  deriving (Eq, Show)

data Instruction
  = Instruction Action Int
  deriving (Eq, Show)

type Instructions = [Instruction]

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

data State = State
  { x :: Int,
    y :: Int,
    rotation :: Int
  }
  deriving (Eq, Show)

pAction :: Parser Action
pAction =
  choice
    [ N <$ char 'N',
      S <$ char 'S',
      E <$ char 'E',
      W <$ char 'W',
      L <$ char 'L',
      R <$ char 'R',
      F <$ char 'F'
    ]

pInstructions :: Parser Instructions
pInstructions = many $ do
  action <- pAction
  value <- decimal
  optional $ void newline
  return $ Instruction action value

doParse :: String -> Either ParserError Instructions
doParse = parse pInstructions "" . pack

rotationAction :: Int -> Action
rotationAction 0 = N
rotationAction 180 = S
rotationAction 90 = E
rotationAction 270 = W

act :: State -> Instruction -> State
act state@State {y} (Instruction N v) =
  state {y = y + v}
act state@State {y} (Instruction S v) =
  state {y = y - v}
act state@State {x} (Instruction E v) =
  state {x = x + v}
act state@State {x} (Instruction W v) =
  state {x = x - v}
act state@State {rotation} (Instruction L v) =
  state {rotation = (rotation - v) `mod` 360}
act state@State {rotation} (Instruction R v) =
  state {rotation = (rotation + v) `mod` 360}
act state@State {rotation} (Instruction F v) =
  act state $ Instruction (rotationAction rotation) v

manhattan :: State -> Int
manhattan State {x, y} = abs x + abs y

initialState :: State
initialState = State {x = 0, y = 0, rotation = 90}

solve :: String -> Either ParserError Int
solve s = do
  instructions <- doParse s
  return $ manhattan $ foldl act initialState instructions

main :: IO (Either ParserError Int)
main = solve <$> actual
