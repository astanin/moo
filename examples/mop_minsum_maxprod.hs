{- A simple multiobjective problem:

  minimize f_1 = x + y
  maximize f_2 = x * y

  s.t. x >= 0, y >=0. -}


import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Constraints
import Moo.GeneticAlgorithm.Multiobjective


import Text.Printf (printf)


mop :: MultiObjectiveProblem ([Double] -> Double)
mop = [ (Minimizing, sum :: [Double] -> Double)
      , (Maximizing, product)]


constraints = [ xvar .>=. 0
              , yvar .>=. 0 ]
xvar [x,_] = x
yvar [_,y] = y


genomes :: [[Double]]
genomes = [[3,3], [9,1], [1,4], [2,2], [1,9], [4,1], [1,1], [4,2]]


popsize :: Int
popsize = 50
step :: StepGA Rand Double
step = withDeathPenalty constraints $
       stepNSGA2bt mop noCrossover (gaussianMutate 0.1 0.5)


main = do
  putStrLn $ "# population size: " ++ show popsize
  let initialize = return . take popsize . cycle $ genomes
  putStrLn $ "# generation\thypervolume(18,0)"
  result <- runIO initialize $
            loopIO [logStats] (Generations 100) step
  putStrLn $ "# best:"
  printPareto result


logStats = DoEvery 20 $ \i pop -> do
             let multiphenotypes = evalAllObjectives mop pop
             printf "# % 8d\t%.3f\n" i (hypervolume mop [18, 0] multiphenotypes)


printPareto result = do
  let paretoGenomes = map takeGenome . takeWhile ((== 1.0) . takeObjectiveValue) $ result
  let paretoObjectives = map takeObjectiveValues $ evalAllObjectives mop paretoGenomes
  putStr $ unlines $
       map (\[x,y] -> printf "%12.3f\t%12.3f" x y ) paretoObjectives