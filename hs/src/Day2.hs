{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day2 where

import Control.Applicative (Alternative)
import Control.Monad (void)
import Data.Either (fromRight)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parse, some, try, (<|>))
import Text.Megaparsec.Char (char, letterChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)

optional :: Alternative f => f a -> f (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

type Parser = Parsec Void Text

type Position = Int

type RequiredChar = Char

type Password = [Char]

data Policy = Policy
  { left :: Position,
    right :: Position,
    required :: RequiredChar
  }
  deriving (Eq, Show)

data Entry = Entry
  { policy :: Policy,
    password :: Password
  }
  deriving (Eq, Show)

type Manifest = [Entry]

entry :: Parser Manifest
entry = do
  many . try $ do
    policy <- do
      left <- decimal
      void (char '-')
      right <- decimal
      void space
      required <- letterChar
      void (string ": ")
      return Policy {..}
    password <- some letterChar
    optional $ void newline
    return Entry {..}

isValid :: Entry -> Bool
isValid Entry {policy = Policy {left, right, required}, password} =
  (password !! pred left == required) /= (password !! pred right == required)

class CountValids a where
  countValids :: a -> Int

instance CountValids Manifest where
  countValids = length . filter isValid

instance CountValids (Either (ParseErrorBundle Text Void) Manifest) where
  countValids = countValids . fromRight []

instance CountValids String where
  countValids = countValids . parse entry "" . pack

parseS :: IO Int
parseS = countValids <$> readFile "day2.txt"
