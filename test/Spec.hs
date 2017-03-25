import Test.HUnit

import BQLTest (bqlTests)

allTests :: Test
allTests =
  TestList $ bqlTests

main :: IO Counts
main =
  runTestTT allTests
