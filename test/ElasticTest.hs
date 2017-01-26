{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module ElasticTest
  ( elasticTests
  ) where

import Test.HUnit
import Database.Bloodhound

import BQL
import Elastic

type TestDescription = String
type FailureMessage  = String
type TestDefinition  = (TestDescription, FailureMessage, BQL, Query)


ageInOut :: [TestDefinition]
ageInOut =
  [ ("Converts Age", "Cannot hande age<90", QAge QLessThan 90, QueryRangeQuery $ RangeQuery (FieldName "age") (RangeDoubleLt $ LessThan 90) (Boost 1.0))
  ]

elasticTests :: [Test]
elasticTests =
  let
    apply :: TestDefinition -> Test
    apply (label, failuresMsg, input, expectation) =
      TestLabel label $ TestCase $ assertEqual failuresMsg expectation (bqlToElastic input)

    testList :: [TestDefinition]
    testList =
      concat
        [ ageInOut
        ]
  in
    map apply testList

