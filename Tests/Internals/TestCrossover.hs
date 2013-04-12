module Tests.Internals.TestCrossover where


import Test.HUnit
import System.Random.Mersenne.Pure64 (pureMT)


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Crossover
import Moo.GeneticAlgorithm.Random



testCrossover =
    TestList
    [ "do N crossovers" ~: do
        let genomes = [[1,1,1,1],[0,0,0,0]] :: [[Int]]
        let result4 = flip evalRandom (pureMT 1) $
                      doNCrossovers 4 genomes (onePointCrossover 0.5)
        let expected4 = [[0,0,1,1],[1,1,0,0],[0,0,0,1],[1,1,1,0]]
        assertEqual "4 crossovers" expected4 result4
        let result3 = flip evalRandom (pureMT 1) $
                      doNCrossovers 3 genomes (onePointCrossover 0.5)
        let expected3 = [[0,0,1,1],[1,1,0,0],[0,0,0,1]]
        assertEqual "3 crossovers" expected3 result3
    , "do all crossovers" ~: do
        let genomes = [[1,1,1,1],[0,0,0,0]] :: [[Int]]
        let result = flip evalRandom (pureMT 1) $
                     doCrossovers genomes (onePointCrossover 0.5)
        let expected = [[1,1,1,0],[0,0,0,1]]
        assertEqual "all crossovers (2 genomes)" expected result
        let genomes3 = [[1,1,1,1],[0,0,0,0],[2,2,2,2]] :: [[Int]]
        -- genes from the last "celibate" genome are lost
        let result3 = filter (==2) . concat . map concat . flip map [0..100] $
                      \i -> flip evalRandom (pureMT i) $
                      doCrossovers genomes (onePointCrossover 1.0)
        assertEqual "discard last genomes without a pair" [] result3
    ]