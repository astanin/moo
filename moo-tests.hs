import System.Exit
import Test.HUnit

import Tests.Internals.TestConstraints (testConstraints)
import Tests.Internals.TestCrossover (testCrossover)
import Tests.Internals.TestSelection (testSelection)
import Tests.Problems.Rosenbrock (testRosenbrock)
import Tests.Internals.TestControl (testControl)

allTests = TestList
  [ testControl
  , testSelection
  , testCrossover
  , testConstraints
  , testRosenbrock ]

main = do
  result <- runTestTT allTests
  if (errors result + failures result) > 0
    then exitFailure
    else exitSuccess
