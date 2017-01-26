{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module BQLTest
  ( bqlTests
  ) where

import Test.HUnit

import Text.Parsec (ParseError)

import BQL


type TestDescription = String
type FailureMessage  = String
type TestInputData   = String
type TestDefinition  = (TestDescription, FailureMessage, TestInputData, Either ParseError BQL)


-- Input Output data to test if the age parser works
ageInOut :: [TestDefinition]
ageInOut =
  [ ("Age =  test", "Cannot handle  =", "age: 24",    Right $ QAge QEqual 24)
  , ("Age >= test", "Cannot handle >=", "age>=34",    Right $ QAge QGreaterThanEqual 34)
  , ("Age <= test", "Cannot handle <=", "age   <=98", Right $ QAge QLessThanEqual 98)
  , ("Age <  test", "Cannot handle  <", "age<   7",   Right $ QAge QLessThan 7)
  ]

-- Input Output data to test if the gender parser works
genderInOut :: [TestDefinition]
genderInOut =
  [ ("Gender Basic Test",      "Cannot do basic conversion", "gender:M",       Right $ QGender Male  )
  , ("Gender Test Whitespace", "Cannot handle whitespace",   "gender   :   F", Right $ QGender Female)
  ]

-- Input Output data to test if the gender parser works
balanceInOut :: [TestDefinition]
balanceInOut =
  [ ("Balance =  test Double",  "Cannot handle  = or doubles", "balance: 24.00", Right $ QBalance QEqual 24)
  , ("Balance >= test Double",  "Cannot handle >= or doubles", "balance>=34.00", Right $ QBalance QGreaterThanEqual 34)
  , ("Balance <= test Integer", "Cannot handle <= or integer", "balance   <=98", Right $ QBalance QLessThanEqual 98)
  , ("Balance <  test Integer", "Cannot handle  < or integer", "balance<   7",   Right $ QBalance QLessThan 7)
  ]

bqlTests :: [Test]
bqlTests =
  let
    apply :: TestDefinition -> Test
    apply (label, failuresMsg, input, expectation) =
      TestLabel label $ TestCase $ assertEqual failuresMsg expectation (parseBQL input)
    
    testList :: [TestDefinition]
    testList =
      concat
        [ ageInOut
        , genderInOut
        , balanceInOut
        ]
  in
    map apply testList

