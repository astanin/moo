import System.Exit
import Test.HUnit

import Tests.Problems.Rosenbrock (testRosenbrock)
import Tests.Internals.TestSelection (testSelection)
import Tests.Internals.TestCrossover (testCrossover)
import Tests.Internals.TestConstraints (testConstraints)

allTests = TestList
  [ testSelection
  , testCrossover
  , testConstraints
  , testRosenbrock ]

main = do
  result <- runTestTT allTests
  if (errors result + failures result) > 0
    then exitFailure
    else exitSuccess
