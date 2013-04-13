module Tests.Internals.TestConstraints where


import Test.HUnit
import System.Random.Mersenne.Pure64 (pureMT)


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Selection
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Constraints



testConstraints =
    TestList
    [ "constrained initialization" ~: do
        let constraints = [ (!!0) .>=. 0
                          , ((-1) .<=..<=. 1) (!!1)
                          , (\([x,y]) -> x+y) .<. 5 ]
        let n = 200
        let genomes = flip evalRandom (pureMT 1) $
                      getConstrainedGenomesRs constraints n (replicate 2 (-10,10::Int))
        assertEqual "exactly n genomes" n $
                    length genomes
        assertEqual "first constraint (>=)" True $
                    all (\([x,_]) -> x >= 0) genomes
        assertEqual "second constraint (<= .. <=)" True $
                    all (\([_,y]) -> (-1) <= y && y <= 1) genomes
        assertEqual "third constraint (<)" True $
                    all (\([x,y]) -> (x+y) < 5) genomes
    ]