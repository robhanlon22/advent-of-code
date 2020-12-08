{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Control.Applicative.Combinators
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void Text

bagParser :: Parser Char
bagParser = C.letterChar <|> C.char ' '

entry :: Parser (String, [(Int, String)])
entry = do
  bag <- M.someTill bagParser (C.string " bags contain ")
  bags <- M.many $ do
    number <- L.decimal
    void C.space
    b <- M.someTill bagParser ((C.string "bag" <|> C.string "bags") `endBy` (C.char ',' <|> C.char '.'))
    return (number, b)
  return (bag, bags)
