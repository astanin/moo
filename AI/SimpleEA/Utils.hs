{-# LANGUAGE BangPatterns #-}
{- |

Some common genetic operators and utilities to work with GA data.

-}

module AI.SimpleEA.Utils (
  -- * Algorithm initialization
    randomGenomes
  , getRandomGenomes
  -- * Selection
  , rouletteSelect
  , tournamentSelect
  , sigmaScale
  , rankScale
  , elite
  -- * Crossover
  , onePointCrossover
  , twoPointCrossover
  , uniformCrossover
  -- * Statistics
  , avgFitness
  , maxFitness
  , minFitness
  , stdDeviation
  -- * Input/output
  , getPlottingData
) where

import Control.Monad (liftM, replicateM, when)
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
-- in the range @(from,to)@. Return a list of genomes and a new state of
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
getRandomGenomes :: (Enum a)
                 => Int -- ^ how many genomes to generate
                 -> Int -- ^ genome length
                 ->  (a, a) -- ^ range of genome bit values
                 -> Rand ([Genome a])
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

-- |Takes a list of fitness values and returns rank scaled values. For
-- a list of /n/ values, this means that the best fitness is scaled to
-- /n/, the second best to /n-1/, and so on.
rankScale :: [Fitness] -> [Fitness]
rankScale fs = map (\n -> max'-fromIntegral n) ranks
    where ranks = (concatMap (`elemIndices` fs) . reverse . nub . sort) fs
          max'  = fromIntegral $ maximum ranks + 1

-- |Fitness-proportionate (roulette-wheel) selection: select a random
-- item from a list of (item, score) where each item's chance of being
-- selected is proportional to its score
rouletteSelect :: [(a, Fitness)] -> Rand a
rouletteSelect xs = do
    let fs = map snd xs  -- fitnesses
    let gs = map fst xs  -- genomes
    let xs' = zip gs (scanl1 (+) fs)
    let sumScores = (snd . last) xs'
    rand <- (sumScores*) `liftM` getDouble
    return $ (fst . head . dropWhile ((rand >) . snd)) xs'

-- |Performs tournament selection amoing @size@ individuals and
-- returns the winner
tournamentSelect :: [(a, Fitness)] -> Int -> Rand a
tournamentSelect xs size = do
    contestants <- randomSample size xs
    let winner = head $ elite contestants
    return winner

-- |Takes a list of (genome,fitness) pairs and returns a list of genomes sorted
-- by fitness (descending)
elite :: [(a, Fitness)] -> [a]
elite = map fst . sortBy (\(_,a) (_,b) -> compare b a)


-- |Select a random point in two genomes, and swap them beyond this point.
-- Apply with probability @p@.
onePointCrossover :: Double -> CrossoverOp a
onePointCrossover p (g1,g2) = do
  t <- getDouble
  if (t < p)
    then do
      r <- getIntR (0, length g1-1)
      let (h1, t1) = splitAt r g1
      let (h2, t2) = splitAt r g2
      return (h1 ++ t2, h2 ++ t1)
    else return (g1,g2)

-- |Select two random points in two genomes, ans swap everything in between.
-- Apply with probability @p@.
twoPointCrossover :: Double -> CrossoverOp a
twoPointCrossover p (g1,g2) = do
  t <- getDouble
  if (t < p)
     then do
       r1 <- getIntR (0, length g1-2)
       r2 <- getIntR (r1+1, length g1-1)
       let (h1, t1) = splitAt r1 g1
       let (m1, e1) = splitAt (r2-r1) t1
       let (h2, t2) = splitAt r1 g2
       let (m2, e2) = splitAt (r2-r1) t2
       return (h1 ++ m2 ++ e1, h2 ++ m1 ++ e2)
     else return (g1, g2)

-- |Swap individual bits of two genomes with probability @p@.
uniformCrossover :: Double -> CrossoverOp a
uniformCrossover p (g1, g2) = unzip `liftM` mapM swap (zip g1 g2)
  where
    swap (x, y) = do
      t <- getDouble
      if (t < p)
         then return (y, x)
         else return (x, y)

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
