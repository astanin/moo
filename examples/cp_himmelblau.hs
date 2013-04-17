{- Constrained Himmelblau function over a non-convex set.


Test problem #1 from Deb, K. (2000). An efficient constraint
handling method for genetic algorithms. Computer methods in applied
mechanics and engineering, 186(2), 311-338.

Unconstrained optimum: (3,2)
Constrained optimum: (2.246826, 2.381865)

Running and visualizing in bash/zsh:

N=100 ; ghc --make cp_himmelblau && ./cp_himmelblau $N > output.txt && ( gnuplot -persist <<< "set view map; unset key ; set isosamples 100 ; set logscale cb ; splot [0:6][0:6] (x**2 + y - 11)**2 + (x + y*y - 7)**2 w pm3d, 'output.txt' u 1:2:(0) w p lc 2 pt 4; set xlabel 'x' ; set ylabel 'y' ; set title 'generation $N' ; replot " ; head -1 output.txt)


-}


import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Constraints


import System.Environment (getArgs)
import Data.Function (on)
import Text.Printf (printf)


f :: [Double] -> Double
f [x, y] = (x**2 + y - 11)**2 + (x + y**2 - 7)**2
xvar [x,_] = x
yvar [_,y] = y
g1 [x,y] = 4.84 - (x-0.05)**2 - (y-2.5)**2
g2 [x,y] = x**2 + (y-2.5)**2 - 4.84


constraints = [ 0 .<= xvar <=. 6
              , 0 .<= yvar <=. 6
              , g1 .>=. 0
              , g2 .>=. 0 ]


popsize = 100
initialize = getRandomGenomesRs popsize [(0,6),(0,6)]
select = withPopulationTransform (fitnessSharing dist 0.025 1 Minimizing) $
         withConstraints constraints (degreeOfViolation 1.0 0.0) Minimizing $
         tournamentSelect Minimizing 2 popsize
step = nextGeneration Minimizing f select 0
       (simulatedBinaryCrossover 0.5)
       (gaussianMutate 0.05 0.025)


dist = distGenotypes `on` takeGenome
    where
      distGenotypes [x1,y1] [x2,y2] = sqrt ((x1-x2)**2 + (y1-y2)**2)


main = do
  -- usage: cp_himmelblau [number-of-generations (default: 100)]
  gens <- return . read . head . (++ ["100"]) =<< getArgs
  result <- runGA initialize (loop (Generations gens) step)
  let best = bestFirst Minimizing . filter (isFeasible constraints . takeGenome) $ result
  if null best
     then putStrLn "# no feasible solutions"
     else putStrLn $ "# best found: [" ++ fmt (head best) ++ "] vs optimal: [2.25 2.38]"
  flip mapM_ result $ \p -> do
         putStrLn $ fmt p

  where
    fmt p = let [x, y] = takeGenome p
            in  printf "%.3f %.3f" x y