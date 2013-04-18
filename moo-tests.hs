import System.Exit
import Test.HUnit

import Tests.Internals.TestFundamentals (testFundamentals)
import Tests.Internals.TestSelection (testSelection)
import Tests.Internals.TestCrossover (testCrossover)
import Tests.Internals.TestConstraints (testConstraints)
import Tests.Internals.TestMultiobjective (testMultiobjective)
import Tests.Problems.Rosenbrock (testRosenbrock)

allTests = TestList
  [ testFundamentals
  , testSelection
  , testCrossover
  , testConstraints
  , testRosenbrock
  , testMultiobjective
  ]

main = do
  result <- runTestTT allTests
  if (errors result + failures result) > 0
    then exitFailure
    else exitSuccess
