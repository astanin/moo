{-# LANGUAGE BangPatterns #-}
{- |

Some common genetic operators and utilities to work with GA data.

-}

module AI.SimpleEA.Utils (
  -- * Encoding
    encodeGray
  , decodeGray
  , encodeBinary
  , decodeBinary
  , encodeGrayReal
  , decodeGrayReal
  , bitsNeeded
  , splitEvery
  -- * Initialization
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
  , blendCrossover
  -- * Mutation
  , pointMutate
  , gaussianMutate
  , clip
  -- * Statistics
  , avgFitness
  , maxFitness
  , minFitness
  , stdDeviation
) where

import AI.SimpleEA.Types
import AI.SimpleEA.Rand
import Codec.Binary.Gray.List
import Control.Monad (liftM, replicateM)
import Control.Monad.Mersenne.Random
import Data.Bits
import Data.List (genericLength, sortBy, nub, elemIndices, sort, foldl')
import System.Random.Mersenne.Pure64


-- | How many bits are needed to represent a range of integer numbers
-- @(from, to)@ (inclusive).
bitsNeeded :: (Integral a, Integral b) => (a, a) -> b
bitsNeeded (from, to) =
    let from' = min from to
        to'= max from to
    in  ceiling . logBase (2::Double) . fromIntegral $ (to' - from' + 1)

-- | Encode an integer number in the range @(from, to)@ (inclusive) as
-- binary sequence of minimal length. Use of Gray code means that a
-- single point mutation leads to incremental change of the encoded
-- value.
encodeGray :: (Bits b, Integral b) => (b, b) -> b -> [Bool]
encodeGray = encodeWithCode gray

-- | Decode a binary sequence using Gray code to an integer in the
-- range @(from, to)@ (inclusive). This is an inverse of 'encodeGray'.
-- Actual value returned may be greater than @to@.
decodeGray :: (Bits b, Integral b) => (b, b) -> [Bool] -> b
decodeGray = decodeWithCode binary

-- | Encode an integer number in the range @(from, to)@ (inclusive)
-- as a binary sequence of minimal length. Use of binary encoding
-- means that a single point mutation may lead to sudden big change
-- of the encoded value.
encodeBinary :: (Bits b, Integral b) => (b, b) -> b -> [Bool]
encodeBinary = encodeWithCode id

-- | Decode a binary sequence to an integer in the range @(from, to)@
-- (inclusive). This is an inverse of 'encodeBinary'.  Actual value
-- returned may be greater than @to@.
decodeBinary :: (Bits b, Integral b) => (b, b) -> [Bool] -> b
decodeBinary = decodeWithCode id

-- | Encode a real number in the range @(from, to)@ (inclusive)
-- width @n@ equally spaced discretization values to binary Gray code.
encodeGrayReal :: (RealFrac a) => (a, a) -> Int -> a -> [Bool]
encodeGrayReal range n = encodeGray (0, n-1) . toDiscreteR range n

-- | Decode a binary sequence using Gray code to a real value in the
-- range @(from, to)@, assuming it was discretized with @n@ equally
-- spaced values.
decodeGrayReal :: (RealFrac a) => (a, a) -> Int -> [Bool] -> a
decodeGrayReal range n = fromDiscreteR range n . decodeGray (0, n-1)

-- | Represent a range @(from, to)@ of real numbers with @n@ equally
-- spaced values.  Use it to discretize a real number @val@.
toDiscreteR :: (RealFrac a)
         => (a, a) -- ^ @(from, to)@, the range to be encoded
         -> Int    -- ^ @n@, how many discrete numbers from the range to consider
         -> a      -- ^ a real number in the range @(from, to)@  to discretize
         -> Int    -- ^ a discrete value (normally in the range @(0, n-1)@)
toDiscreteR range n val =
    let from = uncurry min range
        to = uncurry max range
        dx = (to - from) / (fromIntegral (n - 1))
    in  round $ (val - from) / dx

-- | Take a range @(from, to)@ of real numbers with @n@ equally spaced values.
-- Convert @i@-th value to a real number. This is an inverse of 'toDiscreteR'.
fromDiscreteR :: (RealFrac a)
       => (a, a)  -- ^ @(from, to)@, the encoded range
       -> Int     -- ^ @n@, how many discrete numbers from the range to consider
       -> Int     -- ^ a discrete value in the range @(0, n-1)@
       -> a       -- ^ a real number from the range
fromDiscreteR range n i =
    let from = uncurry min range
        to = uncurry max range
        dx = (to - from) / (fromIntegral (n - 1))
    in  from + (fromIntegral i) * dx

-- | Split a list into pieces of size @n@. This may be useful to split
-- the genome into distinct equally sized “genes” which encode
-- distinct properties of the solution.
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = let (nxs,rest) = splitAt n xs in nxs : splitEvery n rest

encodeWithCode :: (Bits b, Integral b) => ([Bool] -> [Bool]) -> (b, b) -> b -> [Bool]
encodeWithCode code (from, to) n =
    let from' = min from to
        to' = max from to
        nbits = bitsNeeded (from', to')
    in  code . take nbits . toList' $ n - from'

decodeWithCode :: (Bits b, Integral b) => ([Bool] -> [Bool]) -> (b, b) -> [Bool] -> b
decodeWithCode decode (from, to) bits =
    let from' = min from to
    in  (from' +) . fromList . decode $ bits

-- | Generate @n@ random genomes of length @len@ made of elements
-- in the range @(from,to)@. Return a list of genomes and a new state of
-- random number generator.
randomGenomes :: (Enum a) => PureMT -> Int -> Int -> (a, a) ->  ([Genome a], PureMT)
randomGenomes rng n len (from, to) =
    let lo = fromEnum from
        hi = fromEnum to
    in flip runRandom rng $
       (nLists len . map toEnum) `liftM` replicateM (n*len) (getRandomR (lo,hi))
  where nLists :: Int -> [a] -> [[a]]
        nLists _ [] = []
        nLists n ls = let (h,t) = splitAt n ls in h : nLists n t

-- | Generate @n@ random genomes of length @len@ made of elements
-- in the range @(from,to)@. Return a list of genomes.
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

-- |Fitness-proportionate (roulette-wheel) selection: select @n@
-- random items with each item's chance of being selected is
-- proportional to its score.
rouletteSelect :: Int -> SelectionOp a
rouletteSelect n xs = replicateM n roulette1
  where
  fs = map snd xs  -- fitnesses
  gs = map fst xs  -- genomes
  xs' = zip gs (scanl1 (+) fs)
  sumScores = (snd . last) xs'
  roulette1 = do
    rand <- (sumScores*) `liftM` getDouble
    return $ (fst . head . dropWhile ((rand >) . snd)) xs'

-- |Performs tournament selection amoing @size@ individuals and
-- returns the winner. Repeat @n@ times.
tournamentSelect :: Int -- ^ size of the tournament group
                 -> Int -- ^ how many tournaments to run
                 -> SelectionOp a
tournamentSelect size n xs = replicateM n tournament1
  where
  tournament1 = do
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
      r <- getRandomR (0, length g1-1)
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
       r1 <- getRandomR (0, length g1-2)
       r2 <- getRandomR (r1+1, length g1-1)
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

-- | Blend crossover (BLX-alpha) for continuous genetic algorithms.  For
-- each component let @x@ and @y@ be its values in the first and the
-- second parent respectively. Choose corresponding component values
-- of the children independently from the uniform distribution in the
-- range (L,U), where @L = min (x,y) - alpha * d@, @U = max
-- (x,y) + alpha * d@, and @d = abs (x - y)@. @alpha@ is usually
-- 0.5. Takahashi in [10.1109/CEC.2001.934452] suggests 0.366.
blendCrossover :: Double -- ^ alpha, range expansion parameter
               -> CrossoverOp Double
blendCrossover alpha (xs,ys) = unzip `liftM` mapM (blx alpha) (zip xs ys)
  where
    blx a (x,y) =
        let l = min x y - a*d
            u = max x y + a*d
            d = abs (x - y)
        in  do
          x' <- getRandomR (l, u)
          y' <- getRandomR (l, u)
          return (x', y')

-- |Flips a random bit along the length of the genome with probability @p@.
-- With probability @(1 - p)@ the genome remains unaffected.
pointMutate :: Double -> MutationOp Bool
pointMutate p bits = do
  t <- getDouble
  if t < p
     then do
       r <- getRandomR (0, length bits - 1)
       let (before, (bit:after)) = splitAt r bits
       return (before ++ (not bit:after))
     else return bits

-- |For every variable in the genome with probability @p@ replace its
-- value @v@ with @v + sigma*N(0,1)@, where @N(0,1)@ is a normally
-- distributed random variable with mean equal 0 and variance equal 1.
-- With probability @(1 - n*p)@, where @n@ is the number
-- of variables, the genome remains unaffected.
gaussianMutate :: Double -> Double -> MutationOp Double
gaussianMutate p sigma vars = mapM mutate vars
  where
    mutate v  = do
      t <- getDouble
      if t < p
        then do
          n <- getNormal
          return (v + sigma*n)
        else return v

-- |Clip variable @v@ to stay within range @(vmin, vmax)@ (inclusive).
clip :: (Ord a) => (a, a) -> a -> a
clip range v =
    let vmin = uncurry min range
        vmax = uncurry max range
    in  max (min v vmax) vmin

-- |Returns the average fitnesses in a population.
avgFitness :: Population a -> Fitness
avgFitness = avg . map snd
  where avg = uncurry (/) . foldl' (\(!s, !c) x -> (s+x, c+1)) (0, 0)

-- |Returns the maximum fitness in a population.
maxFitness :: Population a -> Fitness
maxFitness = maximum . map snd

-- |Returns the minimum fitness in a population.
minFitness :: Population a -> Fitness
minFitness = minimum . map snd

-- |Returns the standard deviation of the fitness values in a population.
stdDeviation :: Population a -> Double
stdDeviation = sqrt . variance . map snd

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
