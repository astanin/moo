{- constrained problem

   min (x^2 + y^2)

   with x + y >= 1 -}

import System.Environment (getArgs)


import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Constraints


f :: [Double] -> Double
f [x, y] = x*x + y*y


constraints = [ greaterThanOrEqual (\ [x,y] -> x+y ) 1 ]


popsize = 100


initialize = getRandomGenomesRs popsize [(-10,10),(-5,5)]
select = tournamentSelect Minimizing 2 popsize
crossover = unimodalCrossoverRP
mutation = noMutation


step = withDeathPenalty constraints $
       nextGeneration Minimizing f select 2 crossover mutation


main = do
  gens <- return . read . head . (++ ["25"])  =<< getArgs
  result <- runGA initialize $ loop (Generations gens) step
  putStrLn $ "# top 5% after " ++ show gens ++ " generations"
  mapM_ (\[x,y] -> putStrLn $
         show x ++ " " ++ show y ++ " ") $
       map takeGenome . take (popsize `div` 20) . bestFirst Minimizing $ result
