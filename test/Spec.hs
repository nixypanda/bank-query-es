import Test.HUnit

import BQLTest (bqlTests)
import ElasticTest (elasticTests)

allTests :: Test
allTests =
  TestList $ bqlTests ++ elasticTests

main :: IO Counts
main =
  runTestTT allTests
