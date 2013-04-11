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
    [ "tournamentSelect" ~: do
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
    , "rouletteSelect" ~: do
       let gs = map dummyGenome [1, 9]
       let tries = 100 * 1000 :: Int
       let numOfNines = length . filter (==9.0) . map takeObjectiveValue
                        . flip evalRandom (pureMT 1) $ rouletteSelect tries $ gs
       assertEqual "9 is selected from [1,9] 90% of time" 90 (numOfNines `div` 1000)
    , "rankScale" ~: do
        let expected = [([30.0],1.0),([10.0],2.0),([2.0],3.0),([0.0],4.0)]
        let expectedMax = [([0.0],1.0),([2.0],2.0),([10.0],3.0),([30.0],4.0)]
        let result = rankScale Minimizing (map (\x -> ([x],x)) [2,10,0,30])
        let resultMax = rankScale Maximizing (map (\x -> ([x],x)) [2,10,0,30])
        assertEqual "min problem" expected result
        assertEqual "max problem" expectedMax resultMax
    ]