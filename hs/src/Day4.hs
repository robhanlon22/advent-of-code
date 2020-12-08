{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Day4 where

-- import Control.Applicative
-- import Control.Applicative.Permutations
-- import Control.Monad
-- import Data.Char
-- import Data.Maybe
-- import Data.Text (Text)
-- import Data.Void
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- newtype BirthYear = BirthYear Int deriving (Eq, Show)

-- newtype IssueYear = IssueYear Int deriving (Eq, Show)

-- newtype ExpirationYear = ExpirationYear Int deriving (Eq, Show)

-- newtype Height = Height String deriving (Eq, Show)

-- newtype HairColor = HairColor String deriving (Eq, Show)

-- newtype EyeColor = EyeColor String deriving (Eq, Show)

-- newtype PassportId = PassportId String deriving (Eq, Show)

-- newtype CountryId = CountryId String deriving (Eq, Show)

-- data Passport = Passport
--   { byr :: Maybe BirthYear,
--     iyr :: Maybe IssueYear,
--     eyr :: Maybe ExpirationYear,
--     hgt :: Maybe Height,
--     hcl :: Maybe HairColor,
--     ecl :: Maybe EyeColor,
--     pid :: Maybe PassportId,
--     cid :: Maybe CountryId
--   }
--   deriving (Eq, Show)

-- type Parser = Parsec Void Text

-- -- entry = do
-- --   runPermutation $
-- --     (,,) <$> toPermutation (string "aaa")

-- test :: Parser (String, Char, Char)
-- test =
--   intercalateEffect spaceChar $
--     (,,) <$> toPermutation (some (char 'a'))
--       <*> toPermutation (char 'b')
--       <*> toPermutation (char 'c')

-- passportField :: Text -> Parser String
-- passportField fieldName = do
--   void (string fieldName)
--   void (char ':')
--   return $ takeWhile1P (Just "passport field value") (not . isSpace)

-- numValid :: String -> Int
-- numValid = const 5

-- part2 :: IO Int
-- part2 = numValid <$> readFile "../input/day4-actual.txt"
