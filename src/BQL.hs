{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-
   This module defines the Bank Query Language (Lexer and Parser).
 -}

module BQL
  ( BQL(..)
  , Operator(..)
  , Gender(..)
  , parseBQL
  ) where

import Control.Applicative
  (
    pure
  , (<$>)
  , (*>)
  )
import Text.Parsec
  ( ParseError
  , alphaNum
  , char
  , letter
  , try
  )

import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
  (
    LanguageDef
  , caseSensitive
  , commentStart
  , commentEnd
  , commentLine
  , emptyDef
  , identStart
  , identLetter
  , nestedComments
  , reservedNames
  , reservedOpNames
  )
import Text.ParserCombinators.Parsec
  (
    Parser
  , parse
  , (<|>)
  )

---------------------------------------------------------------------------------------------------
-- TYPES ------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

data Operator =
    QLessThan
  | QGreaterThan
  | QLessThanEqual
  | QGreaterThanEqual
  | QEqual
  deriving (Show, Eq)

data Gender =
    Male
  | Female
  deriving (Eq)

instance Show Gender where
  show Male = "M"
  show Female = "F"

data BQL =
    QAge Operator Integer
  | QBalance Operator Double
  | QGender Gender
  deriving (Show, Eq)

---------------------------------------------------------------------------------------------------
-- LEXICAL ANALYSIS -------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- Lexical token spec
kqlDef:: LanguageDef st
kqlDef =
  emptyDef
    { commentStart   = ""
    , commentEnd     = ""
    , commentLine    = ""
    , nestedComments = False
    , identStart     = letter
    , identLetter    = alphaNum
    , reservedNames  =
      [ "balance"
      ]
    , reservedOpNames =
      [ ":"
      , ">"
      , "<"
      , ">="
      , "<="
      ]
    , caseSensitive  = False
    }


-- Creating a lexer
lexer :: Token.TokenParser a
lexer = Token.makeTokenParser kqlDef

-- parses a reserved name
reserved :: String -> Parser ()
reserved = Token.reserved lexer

-- parses an operator
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

-- parses a double
double :: Parser Double
double = Token.float lexer

-- parses an integer
integer :: Parser Integer
integer = Token.integer lexer

---------------------------------------------------------------------------------------------------
-- PARSING ----------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

operator :: Parser Operator
operator =
      (reservedOp ">"  *> pure QGreaterThan)
  <|> (reservedOp "<"  *> pure QLessThan)
  <|> (reservedOp "<=" *> pure QLessThanEqual)
  <|> (reservedOp ">=" *> pure QGreaterThanEqual)
  <|> (reservedOp ":"  *> pure QEqual)

gender :: Parser Gender
gender =
      (char 'M' *> pure Male)
  <|> (char 'F' *> pure Female)

balance :: Parser BQL
balance = QBalance <$> (reserved "balance" *> operator) <*> (try double <|> fmap fromInteger integer)

age :: Parser BQL
age = QAge <$> (reserved "age" *> operator) <*> integer

gender' :: Parser BQL
gender' = QGender <$> (reserved "gender" *> reservedOp ":" *> gender)

bql :: Parser BQL
bql =
      balance
  <|> gender'
  <|> age

parseBQL :: String -> Either ParseError BQL
parseBQL input = parse bql "" input

