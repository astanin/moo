{-# LANGUAGE BangPatterns #-}
{- |

Basic statistics for lists.

-}

module Moo.GeneticAlgorithm.Statistics
  ( average
  , variance
  , quantiles
  , median
  , iqr
  ) where

import Data.List (sort, foldl')

-- |Average
average :: (Num a, Fractional a) => [a] -> a
average = uncurry (/) . foldl' (\(!s, !c) x -> (s+x, c+1)) (0, 0)

-- |Population variance (divided by n).
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


-- | Compute empirical qunatiles (using R type 7 continuous sample quantile).
quantiles :: (Real a, RealFrac a)
             => [a]  -- ^ samples
             -> [a]  -- ^ probabilities in the range (0, 1)
             -> [a]  -- ^ estimated quantiles' values
quantiles xs probs =
    let xs' = sort xs
        n = length xs'
    in  map (quantile7 n xs') probs

-- | Estimate continuous quantile (like R's default type 7, SciPy (1,1), Excel).
quantile7 :: (Real a, RealFrac a)
             => Int       -- ^ @n@ the number of samples
             -> [a]       -- ^ @xs@ samples
             -> a         -- ^ @prob@ numeric probability (0, 1)
             -> a         -- ^ estimated quantile value
quantile7 n xs prob =
    let h = fromIntegral (n-1) * prob + 1
        i = floor h
        x1 = xs !! (i-1)
        x2 = xs !! (i)
    in  case (i >= n, i < 1) of
          (True, _) -> xs !! (i-1) -- prob >= 1
          (_, True) -> xs !! 0     -- prob < 0
          _         -> x1 + (h - fromIntegral i)*(x2 -x1)


-- | Median
median :: (Real a, RealFrac a) => [a] -> a
median xs = head $ quantiles xs [0.5]


-- | Interquartile range.
iqr :: (Real a, RealFrac a) => [a] -> a
iqr xs =
    let [q1,q2] = quantiles xs [0.25, 0.75]
    in  q2 - q1