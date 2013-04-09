module Tests.Internals.TestFundamentals where


import Test.HUnit


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Multiobjective.Types


testFundamentals =
    TestList
    [ "takeGenome" ~: do
        assertEqual "raw genome" [True] $ takeGenome [True]
        assertEqual "phenotype" [True,True] $ takeGenome ([True,True], 42.0::Double)
        assertEqual "multiobjective phenotype" [False] $ takeGenome ([False], [42.0::Double])
    ]