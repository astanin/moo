{- | Some extra facilities to work with 'Rand' monad and 'PureMT'
     random number generator.
-}

module Moo.GeneticAlgorithm.Random
    (
    -- * Random numbers from given range
      getRandomR
    , getRandom
    -- * Probability distributions
    , getNormal2
    , getNormal
    -- * Random samples and shuffles
    , randomSample
    , randomSampleIndices
    , shuffle
    -- * Building blocks
    , withProbability
    -- * Re-exports from random number generator packages
    , getBool, getInt, getWord, getInt64, getWord64, getDouble
    , runRand, evalRand, newPureMT, liftRand
    , Rand, Random, PureMT
    ) where

import Control.Monad (liftM)
import qualified Control.Monad.Random.Strict as MonadRandom
import Control.Monad.Random.Strict (liftRand, runRand, evalRand)
import Data.Complex (Complex (..))
import Data.Int (Int64)
import Data.Word (Word64)
import System.Random (RandomGen, Random(..))
import System.Random.Mersenne.Pure64
import qualified System.Random.Shuffle as S
import qualified Data.Set as Set

type Rand = MonadRandom.Rand PureMT

-- | Yield a new randomly selected value of type @a@ in the range @(lo, hi)@.
-- See 'System.Random.randomR' for details.
getRandomR :: Random a => (a, a) -> Rand a
getRandomR range = liftRand $ \s -> randomR range s

-- | Yield a new randomly selected value of type @a@.
-- See 'System.Random.random' for details.
getRandom :: Random a => Rand a
getRandom = liftRand random

getBool :: Rand Bool
getBool = getRandom
getDouble :: Rand Double
getDouble = getRandom
getWord :: Rand Word
getWord = getRandom
getInt :: Rand Int
getInt = getRandom
getInt64 :: Rand Int64
getInt64 = getRandom
getWord64 :: Rand Word64
getWord64 = getRandom

-- | Yield two randomly selected values which follow standard
-- normal distribution.
getNormal2 :: Rand (Double, Double)
getNormal2 = do
  -- Box-Muller method
  u <- getDouble
  v <- getDouble
  let (c :+ s) = exp (0 :+ (2*pi*v))
  let r = sqrt $ (-2) * log u
  return (r*c, r*s)

-- | Yield one randomly selected value from standard normal distribution.
getNormal :: Rand Double
getNormal = fst `liftM` getNormal2

-- | Take at most n random elements from the list. Preserve order.
randomSample :: Int -> [a] -> Rand [a]
randomSample n xs =
  liftRand $ \g -> select g n (length xs) xs []
  where
    select rng _ _ [] acc = (reverse acc, rng)
    select rng n m xs acc
        | n <= 0     = (reverse acc, rng)
        | otherwise  =
            let (k, rng') = randomR (0, m - n) rng
                (x:rest) = drop k xs
            in  select rng' (n-1) (m-k-1) rest (x:acc)

-- | Select @sampleSize@ numbers in the range from @0@ to @(populationSize-1)@.
-- The function works best when @sampleSize@ is much smaller than @populationSize@.
randomSampleIndices :: Int -> Int -> Rand [Int]
randomSampleIndices sampleSize populationSize =
    liftRand $ \g ->
        let (sampleSet, g') = buildSampleSet g sampleSize Set.empty
        in  (Set.toList sampleSet, g')
  where
    buildSampleSet g n s
        | n <= 0 = (s, g)
        | otherwise =
            let (i, g') = randomR (0, populationSize-1) g
            in  if (i `Set.member` s)
                then buildSampleSet g' n s
                else buildSampleSet g' (n-1) (Set.insert i s)

-- | Randomly reorder the list.
shuffle :: [a] -> Rand [a]
shuffle xs = liftRand $ \g -> randomShuffle xs (length xs) g

-- | Given a sequence (e1,...en) to shuffle, its length, and a random
-- generator, compute the corresponding permutation of the input
-- sequence, return the permutation and the new state of the
-- random generator.
randomShuffle :: RandomGen gen => [a] -> Int -> gen -> ([a], gen)
randomShuffle elements len g =
    let (rs, g') = rseq len g
    in  (S.shuffle elements rs, g')
  where
  -- | The sequence (r1,...r[n-1]) of numbers such that r[i] is an
  -- independent sample from a uniform random distribution
  -- [0..n-i]
  rseq :: RandomGen gen => Int -> gen -> ([Int], gen)
  rseq n g = second lastGen . unzip $ rseq' (n - 1) g
      where
        rseq' :: RandomGen gen => Int -> gen -> [(Int, gen)]
        rseq' i gen
          | i <= 0    = []
          | otherwise = let (j, gen') = randomR (0, i) gen
                        in  (j, gen') : rseq' (i - 1) gen'
        -- apply a function on the second element of a pair
        second :: (b -> c) -> (a, b) -> (a, c)
        second f (x,y) = (x, f y)
        -- the last returned random number generator
        lastGen [] = g   -- didn't use the generator yet
        lastGen (lst:[]) = lst
        lastGen gens = lastGen (drop 1 gens)

-- |Modify value with probability @p@. Return the unchanged value with probability @1-p@.
withProbability :: Double -> (a -> Rand a) -> (a -> Rand a)
withProbability p modify x = do
  t <- getDouble
  if t < p
     then modify x
     else return x
