module Tests.Internals.TestFundamentals where


import Test.HUnit
import System.Random.Mersenne.Pure64 (pureMT)


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Multiobjective.Types
import Moo.GeneticAlgorithm.Random


testFundamentals =
    TestList
    [ "takeGenome" ~: do
        assertEqual "raw genome" [True] $ takeGenome [True]
        assertEqual "phenotype" [True,True] $ takeGenome ([True,True], 42.0::Double)
        assertEqual "multiobjective phenotype" [False] $ takeGenome ([False], [42.0::Double])
    , "withProbability" ~: do
        assertEqual "probability 0" 42 $
                    flip evalRandom (pureMT 1) $
                    withProbability 0 (return . (+1)) 42
        assertEqual "probability 1" 43 $
                    flip evalRandom (pureMT 1) $
                    withProbability 1 (return . (+1)) 42
    ]