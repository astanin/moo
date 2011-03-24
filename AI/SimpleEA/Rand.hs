{-# LANGUAGE BangPatterns #-}

{- | Some extra facilities to work with 'Rand' monad and 'PureMT'
     random number generator.
-}

module AI.SimpleEA.Rand
    (
    -- * Random numbers from given range
      randomIntR
    , getIntR
    -- * Random samples and shuffles
    , randomSample
    , shuffle
    -- * Re-exports from random number generator packages
    , getBool, getInt, getWord, getInt64, getWord64, getDouble
    , runRandom, evalRandom, newPureMT
    , Rand
    ) where

import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64

-- | Yield a new 'Int' value within given range and return this value
-- the new state of random number generator.
randomIntR :: PureMT -> (Int, Int) -> (Int, PureMT)
randomIntR g (lo, hi)
    | lo > hi   = randomIntR g (hi, lo)
    | lo == hi  = (lo, g)
    | otherwise =
        let n = hi - lo + 1
            (r, g') = randomInt g
        in  (lo + r `mod` n, g')

-- | Yield a new 'Int' value within given range. Monadic version of
-- 'randomIntR'.
getIntR :: (Int, Int) -> Rand Int
getIntR (lo,hi) =
  Rand $ \s -> case randomIntR s (lo,hi) of (r, s') -> R r s'

-- | Take at most n random elements from the list. Preserve order.
randomSample :: Int -> [a] -> Rand [a]
randomSample n xs =
  Rand $ \g -> case select g n (length xs) xs [] of (xs', g') -> R xs' g'
  where
    select rng _ _ [] acc = (reverse acc, rng)
    select rng n m xs acc
        | n <= 0     = (reverse acc, rng)
        | otherwise  =
            let (k, rng') = randomIntR rng (0, m - n)
                (x:rest) = drop k xs
            in  select rng' (n-1) (m-k-1) rest (x:acc)


-- | Randomly reorder the list.
shuffle :: [a] -> Rand [a]
shuffle = undefined  -- TODO
