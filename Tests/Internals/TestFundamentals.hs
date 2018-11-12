module Tests.Internals.TestFundamentals where


import Control.Monad (replicateM)
import Test.HUnit
import System.Random.Mersenne.Pure64 (pureMT)


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Multiobjective.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Binary


testFundamentals =
    TestList
    [ "takeGenome" ~: do
        assertEqual "raw genome" [True] $ takeGenome [True]
        assertEqual "phenotype" [True,True] $ takeGenome ([True,True], 42.0::Double)
        assertEqual "multiobjective phenotype" [False] $ takeGenome ([False], [42.0::Double])
    , "withProbability" ~: do
        assertEqual "probability 0" 42 $
                    flip evalRand (pureMT 1) $
                    withProbability 0 (return . (+1)) 42
        assertEqual "probability 1" 43 $
                    flip evalRand (pureMT 1) $
                    withProbability 1 (return . (+1)) 42
    , "pointMutate" ~: do
        let zeros = map (=='1') (replicate 16 '0')
        assertEqual "just 1 bit is changed" (replicate 10 1) $
                    flip evalRand (pureMT 1) $
                         replicateM 10 $
                         return . length . filter id =<< pointMutate 1 zeros
    , "asymmetricMutate" ~: do
        let g = map (=='1') "0000000011111111"  -- 8 bits set
        assertEqual "flip all zeros" 16 $
                    flip evalRand (pureMT 1) $
                         return . length . filter id =<< asymmetricMutate 1 0 g
        assertEqual "flip all ones" 0 $
                    flip evalRand (pureMT 1) $
                         return . length . filter id =<< asymmetricMutate 0 1 g
        assertEqual "flip all" 8 $
                    flip evalRand (pureMT 1) $
                         return . length . filter id =<< asymmetricMutate 1 1 g
    ]
