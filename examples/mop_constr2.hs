{- CONSTR2 problem from (Deb. 2002).
   A part of the unconstrained Pareto-optimal region is not feasible.
-}


import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Constraints
import Moo.GeneticAlgorithm.Multiobjective


popsize = 100
generations = 100


mop :: MultiObjectiveProblem ([Double] -> Double)
mop = [ (Minimizing, \[x1,_] -> x1)
      , (Minimizing, \[x1,x2] -> (1+x2)/x1) ]


constraints = [ 0.1 .<= x1 <=. 1.0
              , 0.0 .<= x2 <=. 5.0
              , g1 .>=. 6.0
              , g2 .>=. 1.0 ]
  where
    x1 [x,_] = x
    x2 [_,y] = y
    g1 [x1,x2] = 9*x1 + x2
    g2 [x1,x2] = 9*x1 - x2



initialize = getConstrainedGenomes constraints popsize [(0.1,1.0),(0.0,5.0)]
tournament = tournamentSelect Minimizing 2 popsize


step :: StepGA Rand Double
step = stepConstrainedNSGA2 constraints (degreeOfViolation 1 0)
       mop tournament (blendCrossover 0.1) noMutation -- (gaussianMutate 0.5 0.5)


main = do
  result <- runGA initialize $ loop (Generations generations) step
  let solutions = map takeGenome $ takeWhile ((<= 10.0) . takeObjectiveValue) result
  let ovs = map takeObjectiveValues $ evalAllObjectives mop solutions
  flip mapM_ ovs $ \[x1,x2] ->
      putStrLn $ show x1 ++ "\t" ++ show x2