module Tests.Internals.TestControl where


import Test.HUnit
import System.Random.Mersenne.Pure64 (pureMT)


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Binary
import Moo.GeneticAlgorithm.Random


instance (Eq a) => Eq (StepResult a) where
    (==) (StopGA xs) (StopGA ys) = xs == ys
    (==) (ContinueGA xs) (ContinueGA ys) = xs == ys
    (==) _ _ = False


testControl =
    TestList
    [ "nextGeneration" ~: do
        let select = tournamentSelect Minimizing 2 8
        let objective = (fromIntegral . length) :: [Int] -> Double
        assertEqual "stop at initial population"  -- initial population is not changed
                    (StopGA [([1],1.0),([2,2],2.0)]) $
                    flip evalRand (pureMT 1) $
                             (nextGeneration Minimizing objective select 0 noCrossover noMutation)
                             (Generations 0) (Left [[1],[2,2]])
        assertEqual "do at least one step"
                    (ContinueGA [([1],1.0),([1],1.0),([1],1.0),([1],1.0)
                                ,([1],1.0),([1],1.0),([1],1.0),([1],1.0)]) $
                    flip evalRand (pureMT 1) $
                             (nextGeneration Minimizing objective select 0 noCrossover noMutation)
                             (Generations 1) (Left [[1],[2,2]])
    ]
