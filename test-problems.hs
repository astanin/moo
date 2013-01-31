import System.Exit
import Test.HUnit

allTests = TestList
  [
  ]

main = do
  result <- runTestTT allTests
  if (errors result + failures result) > 0
    then exitFailure
    else exitSuccess
