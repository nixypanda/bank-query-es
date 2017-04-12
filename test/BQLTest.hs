{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module BQLTest
  ( bqlTests
  ) where

import qualified Data.Map as M

import Test.HUnit
import Text.Parsec (ParseError)

import BQL
  ( BQL(..)
  , Operator(..)
  , parseBQL
  )


type TestDescription = String
type FailureMessage  = String
type TestInputData   = String
type TestDefinition  = (TestDescription, FailureMessage, TestInputData, Either ParseError BQL)


-- Input Output data to test if the age parser works
allInOut :: [TestDefinition]
allInOut =
  [ ( "age = test 1"
    , "Cannot handle = (no space)"
    , "age:24"
    , Right $ B_Num "age" B_Equal 24
    )
  , ( "age = test 2"
    , "Cannot handle = (space)"
    , "age :34"
    , Right $ B_Num "age" B_Equal 34
    )
  , ( "age = test 3"
    , "Cannot handle = (space)"
    , "age   : 98"
    , Right $ B_Num "age" B_Equal 98
    )
  , ( "Gender : test"
    , "Can't handle  :"
    , "gender: F"
    , Right $ B_String "gender" "F"
    )
  , ( "Gender : \" test"
    , "Can't handle \""
    , "gender:M"
    , Right $ B_String "gender" "M"
    )
  ]

parseBQL' :: String -> Either ParseError BQL
parseBQL' = parseBQL M.empty M.empty

bqlTests :: [Test]
bqlTests =
  let
    apply :: TestDefinition -> Test
    apply (label, failuresMsg, input, expectation) =
      TestLabel label $ TestCase $ assertEqual failuresMsg expectation (parseBQL' input)

    testList :: [TestDefinition]
    testList = allInOut
  in
    map apply testList

