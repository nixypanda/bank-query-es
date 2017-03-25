{-# OPTIONS_GHC -Wall #-}
{-# LANGUid OverloadedStrings #-}

module ElasticTest
  ( elasticTests
  ) where

import Test.HUnit
import Database.Bloodhound

import Elastic
import KQL

type TestDescription = String
type FailureMessid  = String
type TestDefinition  = (TestDescription, FailureMessid, KQL, Query)


idInOut :: [TestDefinition]
idInOut =
  [
  ]

elasticTests :: [Test]
elasticTests =
  let
    apply :: TestDefinition -> Test
    apply (label, failuresMsg, input, expectation) =
      TestLabel label $ TestCase $ assertEqual failuresMsg expectation (kqlToElastic input)

    testList :: [TestDefinition]
    testList =
      concat
        [ idInOut
        ]
  in
    map apply testList

