{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-
   This module defines the Bank Query Language (Lexer and Parser).
 -}

module BQL
  ( BQL(..)
  , Operator(..)
  , parseBQL
  ) where

import Control.Applicative (pure, (<$>), (*>))
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time (fromGregorian)
import Data.Text (Text, pack)
import Text.Parsec
  ( ParseError
  , alphaNum
  , between
  , char
  , letter
  , many
  , noneOf
  , oneOf
  , sepBy1
  , spaces
  , try
  )
import Text.ParserCombinators.Parsec (Parser, parse, (<|>))
import Text.ParserCombinators.Parsec.Language
  ( LanguageDef
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

import qualified Data.Map as M
import qualified Text.Parsec.Token as Token

---------------------------------------------------------------------------------------------------
-- TYPES ------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

data Operator =
    B_LessThan
  | B_GreaterThan
  | B_LessThanEqual
  | B_GreaterThanEqual
  | B_Equal
  deriving (Show, Eq)

data BQL =
    B_String Text Text
  | B_MultiString [Text] Text
  | B_Num Text Operator Double
  | B_Date Text Operator UTCTime
  | B_Not BQL
  | B_And [BQL]
  | B_Or [BQL]
  | B_In Text
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
      [ "and"
      , "or"
      , "in"
      ]
    , reservedOpNames =
      [ ":"
      , "-"
      , "="
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

int :: Parser Int
int = fromInteger <$> Token.integer lexer

-- parses surrounding parenthesis: parens p takes care of the parenthesis and
-- uses p to parse what's inside them
parens :: Parser a -> Parser a
parens = Token.parens lexer

-- parses an identifier
identifier :: Parser Text
identifier = fmap pack (Token.identifier lexer)

-- if more formats are need we can add them here
date :: Parser UTCTime
date =
  let
    dayParser = fromGregorian <$> integer <*> (char '-' *> int) <*> (char '-' *> int)
  in
    UTCTime <$> dayParser <*> pure (secondsToDiffTime 0)

-- TODO:
-- email :: Parser Email
-- email =
--   let
--     local = manyTill anyChar (char '@')
--     domain = manyTill anyChar space
--   in
--     Email <$> local <*> domain

fieldValue :: Parser Text
fieldValue =
  let
    escape :: Parser String
    escape = (:) <$> char '\\' <*> ((:) <$> oneOf "\\\"0nrvtbf" <*> pure [])

    nonEscape :: Parser Char
    nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

    character :: Parser String
    character = fmap return nonEscape <|> escape

    parseString :: Parser String
    parseString = concat <$> between (char '"') (char '"') (many character)
 in
   identifier <|> fmap pack parseString


---------------------------------------------------------------------------------------------------
-- PARSING ----------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

operator :: Parser Operator
operator =
      (reservedOp ">"  *> pure B_GreaterThan)
  <|> (reservedOp "<"  *> pure B_LessThan)
  <|> (reservedOp "<=" *> pure B_LessThanEqual)
  <|> (reservedOp ">=" *> pure B_GreaterThanEqual)
  <|> (reservedOp ":"  *> pure B_Equal)


customString :: Parser BQL
customString = B_String <$> (identifier <* reservedOp ":") <*> fieldValue

customNum :: Parser BQL
customNum = B_Num <$> identifier <*> operator <*> (try double <|> fmap fromInteger integer)

customDate :: Parser BQL
customDate = B_Date <$> identifier <*> operator <*> date

kql' :: Parser BQL
kql' = parens'
  <|> not'
  <|> try customDate
  <|> try customNum
  <|> customString
  <|> in'

andQ :: Parser BQL
andQ = B_And <$> sepBy1 kql' (reserved "and" <|> spaces)

orQ :: Parser BQL
orQ = B_Or <$> sepBy1 (andQ <|> kql') (reserved "or")

not' :: Parser BQL
not' = B_Not <$> (reservedOp "-" *> kql')

parens' :: Parser BQL
parens' = parens kql

in' :: Parser BQL
in' = B_In <$> (reserved "in" *> reservedOp ":" *> identifier)

kql :: Parser BQL
kql = orQ

---------------------------------------------------------------------------------------------------
-- EXPANDING --------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

filterMappings :: M.Map Text [Text]
filterMappings =
  M.fromList
    []

-- If you ever need to map single user key to multiple keys on ES
keyMappings :: M.Map Text [Text]
keyMappings =
  M.fromList
    []

getKeys :: Text -> [Text]
getKeys key =
  fromMaybe [key] (M.lookup key keyMappings)

getFilters :: Text -> [Text]
getFilters key =
  fromMaybe [key] (M.lookup key filterMappings)

expand :: BQL -> BQL
expand (B_String field value) =
  case getKeys field of
    [field'] -> B_String field' value
    fields -> B_MultiString fields value
expand (B_Num k op v) = B_Or (fmap (\x -> B_Num x op v) (getKeys k))
expand (B_Date k op v) = B_Or (fmap (\x -> B_Date x op v) (getKeys k))
expand (B_In v) = B_Or (fmap (B_String "object") (getFilters v))
expand (B_Not q) = B_Not (expand q)
expand (B_And xs) = B_And (map expand xs)
expand (B_Or xs) = B_Or (map expand xs)
expand (B_MultiString _ _) = error "expand should never get B_MultiString query"

---------------------------------------------------------------------------------------------------
-- OPTIMIZING -------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

optimizebql :: BQL -> BQL
optimizebql (B_Or [query]) = optimizebql query
optimizebql (B_And [query]) = optimizebql query
optimizebql (B_Or ors) = B_Or (map optimizebql ors)
optimizebql (B_And ands) = B_And (map optimizebql ands)
optimizebql (B_Not notq) = B_Not (optimizebql notq)
optimizebql query = query

---------------------------------------------------------------------------------------------------
-- OPTIMIZED AND PARSED OUTPUT --------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

parseBQL :: String -> Either ParseError BQL
parseBQL = fmap (optimizebql . expand) . parse kql ""

