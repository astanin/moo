module Tests.Internals.TestCrossover where


import Test.HUnit
import System.Random.Mersenne.Pure64 (pureMT)
import Data.List (group, transpose)


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Crossover
import Moo.GeneticAlgorithm.Random



testCrossover =
    TestList
    [ "do N crossovers" ~: do
        let genomes = [[1,1,1,1],[0,0,0,0]] :: [[Int]]
        let result4 = flip evalRand (pureMT 1) $
                      doNCrossovers 4 genomes (onePointCrossover 0.5)
        let expected4 = [[1,0,0,0],[0,1,1,1],[1,1,0,0],[0,0,1,1]]
        assertEqual "4 crossovers" expected4 result4
        let genesums4 = map sum . transpose $ result4
        assertEqual "gene-sums (4 genomes)" [2,2,2,2] genesums4
        let result3 = flip evalRand (pureMT 1) $
                      doNCrossovers 3 genomes (onePointCrossover 0.5)
        let expected3 = [[1,0,0,0],[0,1,1,1],[1,1,0,0]]
        assertEqual "3 crossovers" expected3 result3
    , "do all crossovers" ~: do
        let genomes = [[1,1,1,1],[0,0,0,0]] :: [[Int]]
        let result = flip evalRand (pureMT 1) $
                     doCrossovers genomes (onePointCrossover 0.5)
        let expected = [[1,1,0,0],[0,0,1,1]]
        assertEqual "all crossovers (2 genomes)" expected result
        let genesums2 = map sum . transpose $ result
        assertEqual "gene-sums (2 genomes)" [1,1,1,1] genesums2
        let genomes3 = [[1,1,1,1],[0,0,0,0],[2,2,2,2]] :: [[Int]]
        -- genes from the last "celibate" genome are lost
        let result3 = filter (==2) . concat . map concat . flip map [0..100] $
                      \i -> flip evalRand (pureMT i) $
                      doCrossovers genomes (onePointCrossover 1.0)
        assertEqual "discard last genomes without a pair" [] result3
    , "simple crossover" ~: do
        let ones = replicate 8 1
        let zeros = replicate 8 0
        let genomes = [ones, zeros]
        let n = 1000
        assertEqual "exactly one crossover point" True $
                    all (<=2) . map (length . group) $
                        flip evalRand (pureMT 1) (doNCrossovers n genomes (onePointCrossover 1))
    , "simple crossover" ~: do
        let ones = replicate 8 1
        let zeros = replicate 8 0
        let genomes = [ones, zeros]
        let n = 1000
        assertEqual "exactly one crossover point" True $
                    all (<=2) . map (length . group) $
                        flip evalRand (pureMT 1) (doNCrossovers n genomes (onePointCrossover 1))
    , "two-point crossover" ~: do
        let ones = replicate 8 1
        let zeros = replicate 8 0
        let genomes = [ones, zeros]
        let n = 1000
        assertEqual "exactly two crossover point" True $
                    all (<=3) . map (length . group) $
                        flip evalRand (pureMT 1) (doNCrossovers n genomes (twoPointCrossover 1))
    , "uniform crossover" ~: do
        assertEqual "change all points"
                    ([[0,0,0,0,0,0,0,0,0,0],[1,1,1,1,1,1,1,1,1,1]],[]) $
                    flip evalRand (pureMT 1) $
                             (uniformCrossover 1) [replicate 10 1,replicate 10 (0::Int)]
        assertEqual "change nothing"
                    ([[1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0]],[]) $
                    flip evalRand (pureMT 1) $
                             (uniformCrossover 0) [replicate 10 1,replicate 10 (0::Int)]
        let n = 1000
        let mu = 0.5*n
        let sigma = sqrt(n*0.5*(1-0.5))  -- normal approx to binomial distribution
        let genomes = [ replicate (round n) 1
                      , replicate (round n) 0]
        let xover = uniformCrossover 0.5 :: CrossoverOp Double
        let mkChildren = doNCrossovers 1000 genomes xover :: Rand [Genome Double]
        let children = flip evalRand (pureMT 1) mkChildren :: [Genome Double]
        assertEqual "change approximately half" True $
                    all (\s -> (s >= mu - 4*sigma && s <= mu + 4*sigma)) . map sum $
                        children
    ]
