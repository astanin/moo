module Tests.Internals.TestConstraints where


import Control.Monad (replicateM)
import Test.HUnit
import System.Random.Mersenne.Pure64 (pureMT)


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Selection
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Constraints
import Moo.GeneticAlgorithm.Binary


testConstraints =
    TestList
    [ "constraint satisfaction" ~: do
        let gs =  [[-1],[0],[1],[2],[3::Int]]
        assertEqual ".<." [True, True, False, False, False] $
                    map (isFeasible [head .<. 1]) gs
        assertEqual ".<=." [True, True, True, False, False] $
                    map (isFeasible [head .<=. 1]) gs
        assertEqual ".>." [False, False, False, True, True] $
                    map (isFeasible [head .>. 1]) gs
        assertEqual ".>=." [False, False, True, True, True] $
                    map (isFeasible [head .>=. 1]) gs
        assertEqual ".==." [False, False, True, False, False] $
                    map (isFeasible [head .==. 1]) gs
        assertEqual "non-strict double inequality" [False, True, True, True, False] $
                    map (isFeasible [0 .<= head <=. 2]) gs
        assertEqual "strict double inequality" [False, False, True, False, False] $
                    map (isFeasible [0 .< head <. 2]) gs
    , "constrained initialization" ~: do
        let fI = fromIntegral :: Int -> Double
        let constraints = [ 1 .<= (fI . decodeBinary (0,255)) <=. 42 ]
        let n = 200
        let genomes = flip evalRand (pureMT 1) $
                      getConstrainedBinaryGenomes constraints n 8
        assertEqual "exactly n genomes" n $
                    length genomes
        assertEqual "first constraint (<= .. <=)" True $
                    flip all genomes $ \bits ->
                        let x = fI $ decodeBinary (0,255) bits
                        in (x >= 0) && (x <= (42::Double))
    , "constrained selection (minimizing)" ~: do
        let n = 10
        let tournament2 = tournamentSelect Minimizing 2 n
        let constraints = [head .>=. 0, head .>=. (-1)]
        let ctournament = withConstraints constraints numberOfViolations Minimizing $
                          tournament2
        -- out of two solutions, one violates both constraints, another one only one
        let badvsugly = map (\x -> ([x], x)) [-1, -2]
        -- out of two solutions, one is feasible, the other is not
        let goodvsbad = map (\x -> ([x], x)) [0, -1]
        let result = flip evalRand (pureMT 1) $ ctournament badvsugly
        assertEqual "lesser degree of violation is preferred"
                    (replicate n (-1.0)) $ (map (head . takeGenome) result)
        let result = flip evalRand (pureMT 1) $ ctournament goodvsbad
        assertEqual "feasible solution is preferred"
                    (replicate n (0.0)) $ (map (head . takeGenome) result)
    , "numberOfViolations" ~: do
        let constraints = [head .>=. 0, head .>=. (-1)]
        assertEqual "1 violation" 1 $
                    numberOfViolations constraints [-1]
        assertEqual "2 violations" [2, 2] $
                    map (numberOfViolations constraints) [ [-2], [-3] ]
        assertEqual "no violations" 0 $
                    numberOfViolations constraints [0]
    , "degreeOfViolation" ~: do
        let constraints = [head .>=. 0, (negate . head) .<. (1)]
        assertEqual "no violation" 0 $
                    degreeOfViolation 2.0 0.5 constraints [0]
        assertEqual "1 non-strict violation" 0.25 $
                    degreeOfViolation 2.0 0.5 constraints [-0.5]
        assertEqual "1 non-strict and 1 strict violations" 1.5 $
                    degreeOfViolation 2.0 0.5 constraints [-1.0]
        assertEqual "non-strict double inequality"
                    [3.0,2.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,2.0,3.0] $
                    map (degreeOfViolation 1 0.5 [0 .<= head <=. 6]) $ map (:[]) [-3..9]
        assertEqual "strict double inequality"
                    [3.5,2.5,1.5,0.5,0.0,0.0,0.0,0.0,0.0,0.5,1.5,2.5,3.5] $
                    map (degreeOfViolation 1 0.5 [0 .< head <. 6]) $ map (:[]) [-3..9]
    ]
