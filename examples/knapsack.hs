{-
  The 0-1 knapsack problem. Given a set of items with given weight and value,
  choose which items to put into collection to maximize collection value
  with given maximum weight constraint.

  It is a binary genetic algorithm. This example interleaves computation
  with logging in IO monad.

  To run:

      ghc --make knapsack.hs
      ./knapsack > output.txt

  To visualize the output in gnuplot:

      % gnuplot
      > plot 'output.txt' u 1:2 w l t 'median value', '' u 1:3 w l t 'best value' lt 3
-}

import Moo.GeneticAlgorithm.Binary
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Run
import Moo.GeneticAlgorithm.Selection
import Control.Monad
import Data.List (intercalate)

type Weight = Int
type Value = Int
type Problem = [(Weight, Value)]

items = 42
itemWeight = (1,9 :: Weight)
itemValue = (0,9 :: Value)
maxTotalWeight = items*2 :: Weight

popsize = 11
maxiters = 1000
elitesize = 1

-- fitness function to maximize
totalValue things taken _ = fromIntegral . snd $ totalWeithtAndValue things taken

totalWeithtAndValue :: Problem -> Genome Bool -> (Weight, Value)
totalWeithtAndValue things taken = sumVals (0,0) $ zip taken things
  where
    sumVals (totalW, totalV) ((True, (w,v)):rest)  -- item is taken
        | totalW + w > maxTotalWeight  = (totalW, totalV)  -- weight limit exceeded
        | otherwise                    = sumVals (totalW+w,totalV+v) rest
    sumVals acc ((False, _):rest)      = sumVals acc rest
    sumVals (totalW, totalV) []        = (totalW, totalV)  -- all items in the knapsack


select = tournamentSelect 2 (popsize-elitesize)

logStats :: Problem -> Int -> Population Bool -> IO ()
logStats things iterno pop = do
  let gs = map fst . sortByFitness $ pop  -- genomes
  let best = head gs
  let median = gs !! (length gs `div` 2)
  let bvalue = snd $ totalWeithtAndValue things best
  let mvalue = snd $ totalWeithtAndValue things median
  putStrLn $ intercalate " " (map show [iterno, mvalue, bvalue])

-- generate items to choose from: [(weight, value)]
randomProblem ::  PureMT -> (Problem, PureMT)
randomProblem rng = flip runRandom rng $ do
                      weights <- replicateM items $ getRandomR itemWeight
                      values <- replicateM items $ getRandomR itemValue
                      return $ zip weights values

geneticAlgorithm :: Problem -> PureMT -> IO (Population Bool)
geneticAlgorithm things rng = do
  let (genomes0, rng') = runRandom (replicateM popsize $ replicateM items getRandom) rng
  let fitness = totalValue things
  let pop0 = evalFitness fitness genomes0
  let nextGen = nextGeneration elitesize fitness select (onePointCrossover 0.5) (pointMutate 0.5)
  putStrLn "# generation bestWeight bestValue medianWeight medianValue"
  (pop, rng'') <- loopUntilWithIO (logStats things) (Iteration maxiters) nextGen pop0 rng'
  return pop

main = do
  rng <- newPureMT
  let (things, rng') = randomProblem rng
  pop <- geneticAlgorithm things rng'
  putStrLn "# final population:"
  let best = fst . head . sortByFitness $ pop
  let bestthings = zip best things
  let taken = intercalate ", " . map (showItem . snd) $ filter fst bestthings
  let left = intercalate ", " . map (showItem . snd) $ filter (not . fst) bestthings
  putStrLn $ showPop pop
  putStrLn $ "# taken: " ++ taken
  putStrLn $ "# left: " ++ left

  where
    showPop = intercalate "\n" . map showG
    showG (bs,v) = "# " ++ (concatMap (show . fromEnum) bs) ++ " " ++ show v
    showItem (w, v) = "$" ++ show v ++ "/" ++ show w ++ "oz"
