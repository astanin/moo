module Tests.Internals.TestSelection where


import Test.HUnit
import System.Random.Mersenne.Pure64 (pureMT)


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Selection
import Moo.GeneticAlgorithm.Random



dummyGenome :: Objective -> Phenotype ()
dummyGenome objval = ([], objval)


testSelection =
    TestList
    [ "tournamentSelection" ~: do
        let resultMin = flip evalRandom (pureMT 1) $
                        tournamentSelect Minimizing 3 4 $
                        map dummyGenome [3,2,4]
        let resultMax = flip evalRandom (pureMT 1) $
                        tournamentSelect Maximizing 2 3 $
                        map dummyGenome [2,3]
        let resultMany = flip evalRandom (pureMT 1) $
                         tournamentSelect Maximizing 2 10 $
                         map dummyGenome [1..10]
        assertEqual "4 times best of 3" [2,2,2,2] $
                    map takeObjectiveValue resultMin
        assertEqual "3 times best of 2" [3,3,3] $
                    map takeObjectiveValue resultMax
        assertEqual "10 times best of 4 (seed 1)" [10,10,9,9,10,9,9,9,3,7] $
                    map takeObjectiveValue resultMany
    ]