module Tests.Internals.TestSelection where


import Test.HUnit
import System.Random.Mersenne.Pure64 (pureMT)
import Control.Monad (replicateM)


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Selection
import Moo.GeneticAlgorithm.Random



dummyGenome :: Objective -> Phenotype ()
dummyGenome objval = ([], objval)


testSelection =
    TestList
    [ "tournamentSelect" ~: do
        let resultMin = flip evalRand (pureMT 1) $
                        tournamentSelect Minimizing 3 4 $
                        map dummyGenome [3,2,4]
        let resultMax = flip evalRand (pureMT 1) $
                        tournamentSelect Maximizing 2 3 $
                        map dummyGenome [2,3]
        assertEqual "4 times best of 3" [2,2,2,2] $
                    map takeObjectiveValue resultMin
        assertEqual "3 times best of 2" [3,3,3] $
                    map takeObjectiveValue resultMax
    , "tournamentSelect (10 times best of 4, seed 1)" ~: do
        let times = 10
        let tsize = 4
        let genomes = map dummyGenome [1..10]
        let resultMany = flip evalRand (pureMT 1) $
                         tournamentSelect Maximizing tsize times $
                         genomes
        let objVals = map takeObjectiveValue resultMany
        -- take the same samples again with the same see
        let samples = map (map (genomes !!)) $
                      flip evalRand (pureMT 1) $
                           replicateM times (randomSampleIndices tsize (length genomes))
        assertEqual "maximum is selected every time" (replicate times True)  $
                    zipWith (\selected xs -> selected == (maximum . map takeObjectiveValue $ xs))
                            objVals samples
    , "rouletteSelect" ~: do
       let gs = map dummyGenome [1, 9]
       let tries = 100 * 1000 :: Int
       let numOfNines = length . filter (==9.0) . map takeObjectiveValue
                        . flip evalRand (pureMT 1) $ rouletteSelect tries $ gs
       assertEqual "9 is selected from [1,9] 90% of time" 90 (numOfNines `div` 1000)
    , "stochasticUniversalSampling" ~: do
        let gs = map dummyGenome [2,1]
        let selected = flip evalRand (pureMT 1) $
                       stochasticUniversalSampling 12 gs
        assertEqual "counts are fitness proportional" [4, 8] $
             map length [ (filter ((==1) . takeObjectiveValue) selected)
                        , (filter ((==2) . takeObjectiveValue) selected) ]
    , "rankScale" ~: do
        let expected = [([30.0],1.0),([10.0],2.0),([2.0],3.0),([0.0],4.0)]
        let expectedMax = [([0.0],1.0),([2.0],2.0),([10.0],3.0),([30.0],4.0)]
        let result = rankScale Minimizing (map (\x -> ([x],x)) [2,10,0,30])
        let resultMax = rankScale Maximizing (map (\x -> ([x],x)) [2,10,0,30])
        assertEqual "min problem" expected result
        assertEqual "max problem" expectedMax resultMax
    ]
