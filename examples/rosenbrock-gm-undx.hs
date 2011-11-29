{- Minimize Rosenbrock function using real-valued genetic algorithm.
   Optimal value x* = (1,...,1). F(x*) = 0.
-}

import AI.SimpleEA
import AI.SimpleEA.Utils
import AI.SimpleEA.Rand
import Control.Monad
import Data.List
import Print (printHistoryAndBest)

rosenbrock :: [Double] -> Double
rosenbrock xs = sum . map f $ zip xs (drop 1 xs)
  where
   f (x1, x2) = 100 * (x2 - x1^2)^2 + (x1 - 1)^2

nvariables = 2
xrange = (-30.0, 30.0)
popsize = 100
precision = 1e-4
maxiters = 1000 :: Int

-- fitness function is maximized  when Rosenbrock function is minimized
fitness xs _ = negate $ rosenbrock xs

-- selection: tournament selection with elitism
select pop =
    let keep = popsize `div` 10
        top = take keep (elite pop)
    in  do
      rest <- tournamentSelect 3 (popsize - keep) pop
      return (top ++ rest)

-- Gaussian mutation
mutate =
    let p = 0.5/fromIntegral nvariables
        s = 0.1*(snd xrange - fst xrange)
    in  gaussianMutate p s

-- UNDX crossover
crossover = unimodalCrossover 0.5 (0.35 / sqrt (fromIntegral nvariables))

-- digest: what to log on every iteration
digest pop =
  let m = maxFitness pop
      a = avgFitness pop
  in  (a, m)

main = do
  (pop, log) <- runGA $ do
    -- initial population
    genomes0 <- replicateM popsize $ replicateM nvariables (getRandomR xrange)
    let pop0 = evalFitness fitness genomes0
    -- run genetic algorithm
    loopUntil' (MaxFitness (>= (-precision))
                `Or` Iteration maxiters) digest pop0 $
               nextGeneration fitness select crossover mutate
  printHistoryAndBest show pop log
