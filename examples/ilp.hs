{- Linear Programming problem solved using a GA with integer coding
   (genome is a list of integers).

   The problem is taken from Wikipedia example

   max y
   -x + y <= 1
   3x + 2y <= 12
   2x + 3y <= 12
   x >= 0
   y >= 0

   The optimal soluitions are points (1,2) and (2,2).

-}


import Moo.GeneticAlgorithm.Binary
import Moo.GeneticAlgorithm.Constraints
import Data.List (nub, sort)            -- unique solutions


objective :: Genome Int -> Double
objective [x, y] = fromIntegral y


constraints :: [Constraint Int Int]
constraints = [ (\[x,y] -> y - x) .<=. 1
              , (\[x,y] -> 3*x + 2*y) .<=. 12
              , (\[x,y] -> 2*x + 3*y) .<=. 12
              , (\[x,_] -> x) .>=. 0
              , (\[_,y] -> y) .>=. 0 ]


-- | Replace a random gene @g@ with a random value in the range
-- @[g-radius, g+radius]@.
uniformPointMutation :: (Num a, Random a) => a -> MutationOp a
uniformPointMutation _ [] = return []
uniformPointMutation radius genome = do
  let n = length genome
  i <- getRandomR (0, n-1)
  let (g1,(g:g2)) = splitAt i genome
  g' <- getRandomR (g-radius, g+radius)
  return $ g1 ++ (g':g2)


popsize = 42
selection =
    withConstraints constraints numberOfViolations Maximizing $
    tournamentSelect Maximizing 2 popsize
mutation = withProbability 0.1 $ uniformPointMutation 2
crossover = uniformCrossover 0.5
step =
    withFinalDeathPenalty constraints $
    nextGeneration Maximizing objective selection 0 crossover mutation


main = do
  let start = getConstrainedGenomes constraints popsize [(0,10), (0,10)]
  let stop = IfObjective (\ys -> (minimum ys) >= 2)
             `Or`
             Generations 100
  population <- runGA start (loop stop step)
  let sorted = bestFirst Maximizing $ population
  let bestObj = takeObjectiveValue . head $ sorted
  let solutions = takeWhile (\i -> (takeObjectiveValue i) >= bestObj) sorted
  let uniqSolutions = nub . sort . map takeGenome $ solutions
  print uniqSolutions