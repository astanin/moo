-- | Routines to print results shared between various examples.
module Print where

import Moo.GeneticAlgorithm.Types

import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad (unless, liftM, replicateM, forM_)

-- | Print results of the simulation nicely.
printHistoryAndBest :: ((Genome a) -> String)  -- ^ showGenome function
                    -> Population a  -- ^ final population
                    -> [(Fitness, Fitness)]  -- ^ history of evolution
                    -> IO ()
printHistoryAndBest showGenome pop history = do
    let (best, bestF) = head . sortBy (comparing (negate . snd)) $ pop
    putStrLn "# generation avgFitness maxFitness"
    forM_ (zip [0..] history) $ \(i, (avgF, maxF)) ->
        putStrLn $ unwords [show i, show avgF, show maxF ]
    putStrLn $ "# generations: " ++ show (length history - 1)
    putStrLn $ "# best solution: " ++ showGenome best
    putStrLn $ "# best fitness: " ++ show bestF

-- | Print the best found solution nicely.
printBest :: ((Genome a) -> String)  -- ^ showGenome function
          -> Population a  -- ^ final population
          -> IO ()
printBest showGenome pop = do
    let (best, bestF) = head . sortBy (comparing (negate . snd)) $ pop
    putStrLn $ "# best solution: " ++ showGenome best
    putStrLn $ "# best fitness: " ++ show bestF


