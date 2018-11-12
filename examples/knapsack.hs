{-
  The 0-1 knapsack problem. Given a set of items with given weight and value,
  choose which items to put into collection to maximize collection value
  with given maximum weight constraint.

  It is a binary genetic algorithm. This example interleaves computation
  with logging in IO monad, and terminates by reaching a time limit.

  To run:

      ghc --make knapsack.hs
      ./knapsack > output.txt

  To visualize the output in gnuplot:

      % gnuplot
      > plot 'output.txt' u 1:2 w l t 'median value', '' u 1:3 w l t 'best value' lt 3
-}

import Moo.GeneticAlgorithm.Binary

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
elitesize = 1

-- fitness function to maximize
totalValue :: Problem -> [Bool] -> Objective
totalValue things taken = fromIntegral . snd $ totalWeithtAndValue things taken

totalWeithtAndValue :: Problem -> Genome Bool -> (Weight, Value)
totalWeithtAndValue things taken = sumVals (0,0) $ zip taken things
  where
    sumVals (totalW, totalV) ((True, (w,v)):rest)  -- item is taken
        | totalW + w > maxTotalWeight  = (totalW, totalV)  -- weight limit exceeded
        | otherwise                    = sumVals (totalW+w,totalV+v) rest
    sumVals acc ((False, _):rest)      = sumVals acc rest
    sumVals (totalW, totalV) []        = (totalW, totalV)  -- all items in the knapsack


select = tournamentSelect Maximizing 2 (popsize-elitesize)

-- generate items to choose from: [(weight, value)]
randomProblem ::  IO Problem
randomProblem = do
  rng <- newPureMT
  return . flip evalRand rng $ do
                      weights <- replicateM items $ getRandomR itemWeight
                      values <- replicateM items $ getRandomR itemValue
                      return $ zip weights values

geneticAlgorithm :: Problem -> IO (Population Bool)
geneticAlgorithm things = do
  let initialize = replicateM popsize $ replicateM items getRandom
  let fitness = totalValue things
  let nextGen = nextGeneration Maximizing fitness select elitesize
                          (onePointCrossover 0.5) (pointMutate 0.5)
  runIO initialize $ loopIO
         [DoEvery 10 logStats, TimeLimit 0.1]  -- stop after 100 ms
         (Generations maxBound)  -- effectively, forever; unless an IOHook condition triggers
         nextGen

  where

    logStats :: Int -> Population Bool -> IO ()
    logStats iterno pop = do
      when (iterno == 0) $
           putStrLn "# generation medianValue bestValue"
      let gs = map takeGenome . bestFirst Maximizing $ pop  -- genomes
      let best = head gs
      let median = gs !! (length gs `div` 2)
      let bvalue = snd $ totalWeithtAndValue things best
      let mvalue = snd $ totalWeithtAndValue things median
      putStrLn $ intercalate " " (map show [iterno, mvalue, bvalue])


main = do
  things <- randomProblem
  pop <- geneticAlgorithm things
  putStrLn "# final population:"
  let best = takeGenome . head . bestFirst Maximizing $ pop
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
