import System.Exit
import Test.HUnit

import Tests.Problems.Rosenbrock (testRosenbrock)
import Tests.Internals.TestMultiobjective (testMultiobjective)

allTests = TestList
  [ testMultiobjective
  , testRosenbrock ]

main = do
  result <- runTestTT allTests
  if (errors result + failures result) > 0
    then exitFailure
    else exitSuccess
