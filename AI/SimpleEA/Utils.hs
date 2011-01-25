{- |

Utilitify functions that makes it easier to write the genetic operators and
functions for doing calculations on the EA data.

-}

module AI.SimpleEA.Utils (
    avgFitnesses
  , maxFitnesses
  , minFitnesses
  , stdDeviations
  , randomGenomes
  , fitPropSelect
  , tournamentSelect
  , sigmaScale
  , rankScale
  , elite
  , getPlottingData
) where

import Control.Monad (liftM)
import Control.Monad.Random
import Data.List (genericLength, zip4, sortBy, nub, elemIndices, sort)
import AI.SimpleEA

-- |Returns the average fitnesses for a list of generations.
avgFitnesses :: [[(Genome a, Fitness)]] -> [Fitness]
avgFitnesses = map (\g -> (sum . map snd) g/genericLength g)

-- |Returns the maximum fitness per generation for a list of generations.
maxFitnesses :: [[(Genome a, Fitness)]] -> [Fitness]
maxFitnesses = map (maximum . map snd)

-- |Returns the minimum fitness per generation for a list of generations.
minFitnesses :: [[(Genome a, Fitness)]] -> [Fitness]
minFitnesses = map (minimum . map snd)

-- |Returns the standard deviation of the fitness values per generation fot a
-- list of generations.
stdDeviations :: [[(Genome a, Fitness)]] -> [Double]
stdDeviations = map (stdDev . map snd)

stdDev :: (Floating a) => [a] -> a
stdDev p =
    sqrt (sum sqDiffs/len)
    where len     = genericLength p
          mean    = sum p/len
          sqDiffs = map (\n -> (n-mean)**2) p

-- |Returns an infinite list of random genomes of length @len@ made of elements
-- in the range @[from,to]@
randomGenomes :: (Random a, Enum a) => Int -> a -> a -> StdGen -> [Genome a]
randomGenomes len from to = do
    l <- randomRs (from,to)
    return $ nLists len l
    where nLists :: Int -> [a] -> [[a]]
          nLists _ [] = []
          nLists n ls = take n ls : nLists n (drop n ls)

-- |Applies sigma scaling to a list of fitness values. In sigma scaling, the
-- standard deviation of the population fitness is used to scale the fitness
-- scores.
sigmaScale :: [Fitness] -> [Fitness]
sigmaScale fs = map (\f_g -> 1+(f_g-f_i)/(2*σ)) fs
    where σ   = stdDev fs
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
fitPropSelect :: [(a, Fitness)] -> Rand StdGen a
fitPropSelect xs = do
    let xs' = zip (map fst xs) (scanl1 (+) $ map snd xs)
    let sumScores = (snd . last) xs'
    rand <- getRandomR (0.0, sumScores)
    return $ (fst . head . dropWhile ((rand >) . snd)) xs'

-- |Performs tournament selection amoing @size@ individuals and returns the winner
tournamentSelect :: [(a, Fitness)] -> Int -> Rand StdGen a
tournamentSelect xs size = do
    let l = length xs
    rs <- liftM (take size . nub) $ getRandomRs (0,l-1)
    let contestants = map (xs!!) rs
    let winner = head $ elite contestants
    return winner

-- |takes a list of (genome,fitness) pairs and returns a list of genomes sorted
-- by fitness (descending)
elite :: [(a, Fitness)] -> [a]
elite = map fst . sortBy (\(_,a) (_,b) -> compare b a)

-- |takes a list of generations and returns a string intended for plotting with
-- gnuplot.
getPlottingData :: [[(Genome a, Fitness)]] -> String
getPlottingData gs = concatMap conc (zip4 ns fs ms ds)
    where ns = [1..] :: [Int]
          fs = avgFitnesses gs
          ms = maxFitnesses gs
          ds = stdDeviations gs
          conc (n, a, m ,s) =
              show n ++ " " ++ show a ++ " " ++ show m ++ " " ++ show s ++ "\n"
