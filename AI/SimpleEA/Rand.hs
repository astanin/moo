{-# LANGUAGE BangPatterns #-}

{- | Some extra facilities to work with 'Rand' monad and 'PureMT'
     random number generator.
-}

module AI.SimpleEA.Rand
    (
    -- * Random numbers from given range
      getRandomR
    , getRandom
    -- * Random samples and shuffles
    , randomSample
    , shuffle
    -- * Re-exports from random number generator packages
    , getBool, getInt, getWord, getInt64, getWord64, getDouble
    , runRandom, evalRandom, newPureMT
    , Rand, Random
    ) where

import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64
import qualified System.Random.Shuffle as S
import System.Random (RandomGen, Random(..))

-- | Yield a new randomly selected value of type @a@ in the range @(lo, hi)@.
-- See 'System.Random.randomR' for details.
getRandomR :: Random a => (a, a) -> Rand a
getRandomR range = Rand $ \s -> let (r, s') = randomR range s in R r s'

-- | Yield a new randomly selected value of type @a@.
-- See 'System.Random.random' for for details.
getRandom :: Random a => Rand a
getRandom = Rand $ \g -> let (r, g') = random g in R r g'

-- | Take at most n random elements from the list. Preserve order.
randomSample :: Int -> [a] -> Rand [a]
randomSample n xs =
  Rand $ \g -> case select g n (length xs) xs [] of (xs', g') -> R xs' g'
  where
    select rng _ _ [] acc = (reverse acc, rng)
    select rng n m xs acc
        | n <= 0     = (reverse acc, rng)
        | otherwise  =
            let (k, rng') = randomR (0, m - n) rng
                (x:rest) = drop k xs
            in  select rng' (n-1) (m-k-1) rest (x:acc)


-- | Randomly reorder the list.
shuffle :: [a] -> Rand [a]
shuffle xs = Rand $ \g ->
             let (xs', g') = randomShuffle xs (length xs) g in  R xs' g'

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

