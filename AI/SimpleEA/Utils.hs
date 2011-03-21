{-# LANGUAGE BangPatterns #-}
{- |

Utilitify functions that makes it easier to write the genetic operators and
functions for doing calculations on the EA data.

-}

module AI.SimpleEA.Utils (
  -- * Algorithm initialization
    randomGenomes
  , getRandomGenomes
  -- * Selection
  , fitPropSelect
  , tournamentSelect
  , sigmaScale
  , rankScale
  , elite
  -- * Statistics
  , avgFitness
  , maxFitness
  , minFitness
  , stdDeviation
  -- * Input/output
  , getPlottingData
) where

import Control.Monad (liftM, replicateM)
import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64
import AI.SimpleEA.Rand
import Data.List (genericLength, zip4, sortBy, nub, elemIndices, sort, foldl')
import AI.SimpleEA

-- |Returns the average fitnesses in a population.
avgFitness :: [(Genome a, Fitness)] -> Fitness
avgFitness = avg . map snd
  where avg = uncurry (/) . foldl' (\(!s, !c) x -> (s+x, c+1)) (0, 0)

-- |Returns the maximum fitness in a population.
maxFitness :: [(Genome a, Fitness)] -> Fitness
maxFitness = maximum . map snd

-- |Returns the minimum fitness in a population.
minFitness :: [(Genome a, Fitness)] -> Fitness
minFitness = minimum . map snd

-- |Returns the standard deviation of the fitness values in a population.
stdDeviation :: [(Genome a, Fitness)] -> Double
stdDeviation = sqrt . variance . map snd

-- Population variance (divided by n).
variance :: (Floating a) => [a] -> a
variance xs = let (n, _, q) = foldr go (0, 0, 0) xs
              in  q / fromIntegral n
    where
    -- Algorithm by Chan et al.
    -- ftp://reports.stanford.edu/pub/cstr/reports/cs/tr/79/773/CS-TR-79-773.pdf
    go :: Floating a => a -> (Int, a, a) -> (Int, a, a)
    go x (n, sa, qa)
        | n == 0 = (1, x, 0)
        | otherwise =
            let na = fromIntegral n
                delta = x - sa/na
                sa' = sa + x
                qa' = qa + delta*delta*na/(na+1)
            in  (n + 1, sa', qa')

-- | Generate @n@ random genomes of length @len@ made of elements
-- in the range @(from,to). Return a list of genomes and a new state of
-- random number generator.
randomGenomes :: (Enum a) => PureMT -> Int -> Int -> (a, a) ->  ([Genome a], PureMT)
randomGenomes rng n len (from, to) =
    let lo = fromEnum from
        hi = fromEnum to
    in flip runRandom rng $
         (nLists len . map toEnum) `liftM` replicateM (n*len) (getIntR (lo,hi))
  where nLists :: Int -> [a] -> [[a]]
        nLists _ [] = []
        nLists n ls = let (h,t) = splitAt n ls in h : nLists n t

-- | Monadic version of 'randomGenomes'.
getRandomGenomes :: (Enum a) => Int -> Int ->  (a, a) -> Rand ([Genome a])
getRandomGenomes n len range = Rand $ \rng ->
                               let (gs, rng') = randomGenomes rng n len range
                               in  R gs rng'

-- |Applies sigma scaling to a list of fitness values. In sigma scaling, the
-- standard deviation of the population fitness is used to scale the fitness
-- scores.
sigmaScale :: [Fitness] -> [Fitness]
sigmaScale fs = map (\f_g -> 1+(f_g-f_i)/(2*σ)) fs
    where σ   = sqrt . variance $ fs
          f_i = sum fs/genericLength fs

-- |Takes a list of fitness values and returns rank scaled values. For a list of /n/ values, this
-- means that the best fitness is scaled to /n/, the second best to /n-1/, and so on.
rankScale :: [Fitness] -> [Fitness]
rankScale fs = map (\n -> max'-fromIntegral n) ranks
    where ranks = (concatMap (`elemIndices` fs) . reverse . nub . sort) fs
          max'  = fromIntegral $ maximum ranks + 1

-- |Fitness-proportionate selection: select a random item from a list of (item,
-- score) where each item's chance of being selected is proportional to its
-- score
fitPropSelect :: [(a, Fitness)] -> Rand a
fitPropSelect xs = do
    let xs' = zip (map fst xs) (scanl1 (+) $ map snd xs)
    let sumScores = (snd . last) xs'
    rand <- (sumScores*) `liftM` getDouble
    return $ (fst . head . dropWhile ((rand >) . snd)) xs'

-- |Performs tournament selection amoing @size@ individuals and returns the winner
tournamentSelect :: [(a, Fitness)] -> Int -> Rand a
tournamentSelect xs size = do
    contestants <- randomSample size xs
    let winner = head $ elite contestants
    return winner

-- |Takes a list of (genome,fitness) pairs and returns a list of genomes sorted
-- by fitness (descending)
elite :: [(a, Fitness)] -> [a]
elite = map fst . sortBy (\(_,a) (_,b) -> compare b a)

-- |Takes a list of generations and returns a string intended for plotting with
-- gnuplot.
getPlottingData :: [[(Genome a, Fitness)]] -> String
getPlottingData gs = concatMap conc (zip4 ns fs ms ds)
    where ns = [1..] :: [Int]
          fs = map avgFitness gs
          ms = map maxFitness gs
          ds = map stdDeviation gs
          conc (n, a, m ,s) =
              show n ++ " " ++ show a ++ " " ++ show m ++ " " ++ show s ++ "\n"
