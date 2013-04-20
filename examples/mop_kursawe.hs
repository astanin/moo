{- Kursawe function

A multiobjective optimization problem with a discontinuous and
non-convex Pareto front.

Kursawe, F. (1991). A variant of evolution strategies for vector
optimization. In Parallel Problem Solving from Nature
(pp. 193-197). Springer Berlin Heidelberg.

-}


import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Constraints
import Moo.GeneticAlgorithm.Multiobjective


n = 3
popsize = 100
generations = 100


mop :: MultiObjectiveProblem ([Double] -> Double)
mop = [ (Minimizing,
         \xs -> sum (map (\i -> -10*exp(-0.2*sqrt(((xs!!i)**2 + (xs!!(i+1))**2)))) [0..(n-2)]))
      , (Minimizing,
         \xs -> sum (map (\x -> abs(x)**0.8 + 5*sin(x**3)) xs)) ]


constraints :: [Constraint Double Double]
constraints = [ (-5.0) .<= (!!0) <=. 5.0
              , (-5.0) .<= (!!1) <=. 5.0
              , (-5.0) .<= (!!2) <=. 5.0 ]


initialize = getRandomGenomes popsize (replicate 3 (-5.0, 5.0))


step :: StepGA Rand Double
step = stepConstrainedNSGA2bt constraints (degreeOfViolation 1 0)
       mop unimodalCrossoverRP (gaussianMutate 0.01 0.5)


main = do
  result <- runGA initialize $ loop (Generations generations) step
  let solutions = map takeGenome $ takeWhile ((<= 10.0) . takeObjectiveValue) result
  let ovs = map takeObjectiveValues $ evalAllObjectives mop solutions
  flip mapM_ ovs $ \[x1,x2] ->
      putStrLn $ show x1 ++ "\t" ++ show x2