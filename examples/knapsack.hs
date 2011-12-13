{-
  The 0-1 knapsack problem. Given a set of items with given weight and value,
  choose which items to put into collection to maximize collection value
  with given maximum weight constraint.
-}

import AI.SimpleEA
import AI.SimpleEA.Utils
import AI.SimpleEA.Rand
import Control.Monad
import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64
import Data.List (intercalate)

import Print (printHistoryAndBest)

type Weight = Int
type Value = Int

items = 50
itemWeight = (1,9 :: Weight)
itemValue = (0,9 :: Value)
maxTotalWeight = items*2 :: Weight

popsize = 11
maxiters = 100
elitesize = 1

-- fitness function to maximize
totalValue things taken _ = fromIntegral . sumVals (0,0) $ zip taken things
  where
    sumVals (totalW, totalV) ((True, (w,v)):rest)  -- item is taken
        | totalW + w > maxTotalWeight  = totalV  -- weight limit exceeded
        | otherwise                    = sumVals (totalW+w,totalV+v) rest
    sumVals acc ((False, _):rest)      = sumVals acc rest
    sumVals (_, totalV) []             = totalV   -- all items in the knapsack


bestValue :: Population Bool -> Fitness
bestValue = maximum . map snd

select = tournamentSelect 2 (popsize-elitesize)

geneticAlgorithm :: [(Weight,Value)] -> Rand (Population Bool, [Fitness])
geneticAlgorithm things = do
  genomes0 <- replicateM popsize $ replicateM items getRandom
  let fitness = totalValue things
  let pop0 = evalFitness fitness genomes0
  let bestValue = maximum . map snd
  loopUntil' (Iteration maxiters) bestValue pop0 $ do
      nextGeneration elitesize fitness select
                     (onePointCrossover 0.5) (pointMutate 0.5)

main = do
  rng <- newPureMT
  -- generate items to choose from: [(weight, value)]
  let (things, rng') = flip runRandom rng $ do
                         weights <- replicateM items $ getRandomR itemWeight
                         values <- replicateM items $ getRandomR itemValue
                         return $ zip weights values
  let (pop,log) = flip evalRandom rng' $ geneticAlgorithm things
  -- print results and evolution log
  putStrLn "# iteration bestValue"
  mapM_ (\(i,v) -> putStrLn $ show i ++ " " ++ show v) (zip [0..] log)
  let best = fst . head . sortByFitness $ pop
  let bestthings = zip best things
  let taken = intercalate ", " . map (show . snd) $ filter fst bestthings
  let left = intercalate ", " . map (show . snd) $ filter (not . fst) bestthings
  putStrLn $ showPop pop
  putStrLn $ "# taken: " ++ taken
  putStrLn $ "# left: " ++ left

  where
    showPop = intercalate "\n" . map showG
    showG (bs,v) = "# " ++ (concatMap (show . fromEnum) bs) ++ " " ++ show v
