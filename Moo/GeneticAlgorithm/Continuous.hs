{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- |

Continuous (real-coded) genetic algorithms. Candidate solutions are
represented as lists of real variables.

-}


module Moo.GeneticAlgorithm.Continuous
  (
  -- * Types
    module Moo.GeneticAlgorithm.Types

  -- * Initialization
  , getRandomGenomes
  , uniformGenomes

  -- * Selection
  , rouletteSelect
  , stochasticUniversalSampling
  , tournamentSelect
  -- ** Scaling and niching
  , withPopulationTransform
  , withScale
  , rankScale
  , withFitnessSharing
  , distance1, distance2, distanceInf
  -- ** Sorting
  , bestFirst

  -- * Crossover
  -- ** Neighborhood-based operators
  , blendCrossover
  , unimodalCrossover
  , unimodalCrossoverRP
  , simulatedBinaryCrossover
  , module Moo.GeneticAlgorithm.Crossover

  -- * Mutation
  , gaussianMutate

  -- * Control
  , module Moo.GeneticAlgorithm.Random
  , module Moo.GeneticAlgorithm.Run
) where

import Control.Monad (liftM, replicateM)
import Data.List (genericLength, foldl')

import Moo.GeneticAlgorithm.Crossover
import Moo.GeneticAlgorithm.LinAlg
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Selection
import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Run
import Moo.GeneticAlgorithm.Utilities (getRandomGenomes)


-- | Generate at most @popsize@ genomes uniformly distributed in @ranges@.
uniformGenomes :: Int -> [(Double,Double)] -> [Genome Double]
uniformGenomes popsize ranges =
    let dims = map (uncurry subtract) ranges :: [Double]
        ndims = length dims :: Int
        vol = product dims
        mdim = vol ** (1.0/fromIntegral ndims) :: Double
        msamples = (fromIntegral popsize) ** (1.0/fromIntegral ndims) :: Double
        ptsPerDim = map (\d -> round $ d*msamples/mdim) dims :: [Int]
        ptsInLastDims = product $ drop 1 ptsPerDim :: Int
        ptsInFirstDim = popsize `div` ptsInLastDims :: Int
        ptsPerDim' = ptsInFirstDim : (drop 1 ptsPerDim) :: [Int]
        linspaces = zipWith linspace ranges ptsPerDim' :: [[Double]]
    in  sproduct [[]] linspaces
  where
    linspace :: (Double, Double) -> Int -> [Double]
    linspace (lo, hi) n = map (\i -> (fromIntegral i)*(hi-lo)/fromIntegral (n-1)) [0..n-1]
    sproduct :: [[Double]] -> [[Double]] ->  [[Double]]
    sproduct gs [] = gs
    sproduct gs (l:ls) =
           let gs' = [x:g | g<-gs, x<-l]
           in  sproduct gs' ls


-- | 1-norm distance: @sum |x_i - y-i|@.
distance1 :: (Num a) => [a] -> [a] -> a
distance1 xs ys = sum . map abs $ zipWith (-) xs ys


-- | 2-norm distance: @(sum (x_i - y_i)^2)^(1/2)@.
distance2 :: (Floating a) => [a] -> [a] -> a
distance2 xs ys = sqrt . sum . map (^(2::Int)) $ zipWith (-) xs ys


-- | Infinity norm distance: @max |x_i - y_i|@.
distanceInf :: (Real a) => [a] -> [a] -> a
distanceInf xs ys = maximum . map abs $ zipWith (-) xs ys


-- | Blend crossover (BLX-alpha) for continuous genetic algorithms.  For
-- each component let @x@ and @y@ be its values in the first and the
-- second parent respectively. Choose corresponding component values
-- of the children independently from the uniform distribution in the
-- range (L,U), where @L = min (x,y) - alpha * d@, @U = max
-- (x,y) + alpha * d@, and @d = abs (x - y)@. @alpha@ is usually
-- 0.5. Takahashi in [10.1109/CEC.2001.934452] suggests 0.366.
blendCrossover :: Double -- ^ @alpha@, range expansion parameter
               -> CrossoverOp Double
blendCrossover _ [] = return ([], [])
blendCrossover _ [celibate] = return ([],[celibate])
blendCrossover alpha (xs:ys:rest) = do
  (xs',ys') <- unzip `liftM` mapM (blx alpha) (zip xs ys)
  return ([xs',ys'], rest)
  where
    blx a (x,y) =
        let l = min x y - a*d
            u = max x y + a*d
            d = abs (x - y)
        in  do
          x' <- getRandomR (l, u)
          y' <- getRandomR (l, u)
          return (x', y')

-- | Unimodal normal distributed crossover (UNDX) for continuous
-- genetic algorithms. Recommended parameters according to [ISBN
-- 978-3-540-43330-9] are @sigma_xi = 0.5@, @sigma_eta =
-- 0.35/sqrt(n)@, where @n@ is the number of variables (dimensionality
-- of the search space). UNDX mixes three parents, producing normally
-- distributed children along the line between first two parents, and using
-- the third parent to build a supplementary orthogonal correction
-- component.
--
-- UNDX preserves the mean of the offspring, and also the
-- covariance matrix of the offspring if @sigma_xi^2 = 0.25@.  By
-- preserving distribution of the offspring, /the UNDX can efficiently
-- search in along the valleys where parents are distributed in
-- functions with strong epistasis among parameters/ (idem).
unimodalCrossover :: Double  -- ^ @sigma_xi@, the standard deviation of
                            -- the mix between two principal parents
                  -> Double  -- ^ @sigma_eta@, the standard deviation
                            -- of the single orthogonal component
                  -> CrossoverOp Double
unimodalCrossover sigma_xi sigma_eta (x1:x2:x3:rest) = do
  let d = x2 `minus` x1  -- vector between parents
  let x_mean = 0.5 `scale` (x1 `plus` x2)  -- parents' average
   -- distance to the 3rd parent in the orthogonal subspace
  let dist3 =
          let v31 = x3 `minus` x1
              v21 = x2 `minus` x1
              base = norm2 v21
              -- twice the triangle area
              area = sqrt $ (dot v31 v31)*(dot v21 v21) - (dot v21 v31)^(2::Int)
              h = area / base
          in  if isNaN h    -- if x1 and x2 coincide
                then norm2 v31
                else h
  let n = length x1
  (parCorr, orthCorrs) <-
      if norm2 d > 1e-6
      then do -- distinct parents
        let exs = drop 1 . mkBasis $ d
        getNormals n >>= \case
          (xi:etas) -> let
              xi' = sigma_xi * xi
              parCorr = xi' `scale` d
              etas' = map (dist3 * sigma_eta *) etas
              orthCorrs = zipWith scale etas' exs
            in return (parCorr, orthCorrs)
          _ -> error "Parameters too short"
      else do -- identical parents, direction d is undefined
        let exs = map (basisVector n) [0..n-1]
        etas <- getNormals n
        let etas' = map (dist3 * sigma_eta *) etas
        let orthCorrs = zipWith scale etas' exs
        let zeroCorr = replicate n 0.0
        return (zeroCorr, orthCorrs)
  let totalCorr = foldr plus parCorr orthCorrs
  let child1 = x_mean `minus` totalCorr
  let child2 = x_mean `plus` totalCorr
  -- drop only two parents of the three, to keep the number of children the same
  return ([child1, child2], x3:rest)
  where
    -- generate a list of n normally distributed random vars
    getNormals n = do
      ps <- replicateM ((n + 1) `div` 2) getNormal2
      return . take n $ concatMap (\(x,y) -> [x,y]) ps
    -- i-th basis vector in n-dimensional space
    basisVector n i = replicate (n-i-1) 0.0 ++ [1] ++ replicate i 0.0
    -- generate orthonormal bases starting from direction dir0
    mkBasis :: [Double] -> [[Double]]
    mkBasis dir0 =
        let n = length dir0
            dims = [0..n-1]
            ixs = map (basisVector n) dims
        in  map normalize . reverse $ foldr build [dir0] ixs
      where
        build ix exs =
            let projs = map (proj ix) exs
                rem = foldl' minus ix projs
            in  if norm2 rem <= 1e-6 * maximum (map norm2 exs)
                then exs   -- skip this vector, as linear depenent with dir0
                else rem : exs  -- add to the list of orthogonalized vectors
unimodalCrossover _ _ [] = return ([], [])
unimodalCrossover _ _ (x1:x2:[]) = return ([x1,x2], [])  -- FIXME the last two
unimodalCrossover _ _ [celibate]  = return ([], [celibate])

-- | Run 'unimodalCrossover' with default recommended parameters.
unimodalCrossoverRP :: CrossoverOp Double
unimodalCrossoverRP [] = return ([], [])
unimodalCrossoverRP parents@(x1:_) =
    let n = genericLength x1
        sigma_xi = 0.5
        sigma_eta = 0.35 / sqrt n
    in  unimodalCrossover sigma_xi sigma_eta parents

-- | Simulated binary crossover (SBX) operator for continuous genetic
-- algorithms. SBX preserves the average of the parents and has a
-- spread factor distribution similar to single-point crossover of the
-- binary genetic algorithms. If @n > 0@, then the heighest
-- probability density is assigned to the same distance between
-- children as that of the parents.
--
-- The performance of real-coded genetic algorithm with SBX is similar
-- to that of binary GA with a single-point crossover. For details see
-- Simulated Binary Crossover for Continuous Search Space (1995) Agrawal etal.
simulatedBinaryCrossover :: Double  -- ^ non-negative distribution
                                   -- parameter @n@, usually in the
                                   -- range from 2 to 5; for small
                                   -- values of @n@ children far away
                                   -- from the parents are more likely
                                   -- to be chosen.
                         -> CrossoverOp Double
simulatedBinaryCrossover n (x1:x2:rest) = do
  -- let pdf beta | beta >  1.0 = 0.5*(n+1)/beta**(n+2)
  --              | beta >= 0.0 = 0.5*(n+1)*beta**n
  --              | otherwise   = 0.0   -- beta < 0
  let cdf beta | beta < 0    = 0.0
               | beta <= 1.0 = 0.5*beta**(n+1)
               | otherwise   = 1.0-0.5/beta**(n+1)  -- beta > 1.0
  u <- getDouble  -- uniform random variable in [0,1]
  -- solve cdf(beta) = u with absolute residual less than eps > 0
  let solve eps u = solve' 0.0 (upperB 2.0)
        where
          upperB b | cdf b < u = upperB (b*2)
                   | otherwise = b
          solve' b1 b2 =
              let b = 0.5*(b1+b2)
                  r = cdf b - u
              in  if abs r < eps
                  then b
                  else
                      if r >= 0
                      then solve' b1 b
                      else solve' b b2
  let beta = solve 1e-6 u
  let xmean = 0.5 `scale` (x1 `plus` x2)
  let deltax = (0.5 * beta) `scale` (x2 `minus` x1)
  let c1 = xmean `plus`  deltax
  let c2 = xmean `minus` deltax
  return ([c1,c2], rest)
simulatedBinaryCrossover _ celibates = return ([], celibates)


-- |For every variable in the genome with probability @p@ replace its
-- value @v@ with @v + sigma*N(0,1)@, where @N(0,1)@ is a normally
-- distributed random variable with mean equal 0 and variance equal 1.
-- With probability @(1 - p)^n@, where @n@ is the number
-- of variables, the genome remains unaffected.
gaussianMutate :: Double  -- ^ probability @p@
               -> Double  -- ^ @sigma@
               -> MutationOp Double
gaussianMutate p sigma vars = mapM mutate vars
  where
    mutate = withProbability p $ \v -> do
               n <- getNormal
               return (v + sigma*n)
