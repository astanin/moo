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
  , withElite
  , sortByFitness
  , withScale
  , sigmaScale
  , rankScale
  -- * Crossover
  , onePointCrossover
  , twoPointCrossover
  , uniformCrossover
  , blendCrossover
  , unimodalCrossover
  , unimodalCrossoverRP
  , simulatedBinaryCrossover
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
import Data.List (genericLength, sortBy, foldl')
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


-- | Apply given scaling or other transform to population before selection.
withScale :: (Population a -> Population a) -> SelectionOp a -> SelectionOp a
withScale scale select = \pop -> select (scale pop)

-- | Sigma scaling. Fitness values of all genomes are scaled with
-- respect to standard devation of population fitness.
sigmaScale :: Population a -> Population a
sigmaScale pop = map (\(g,f) -> (g,1+(f-f_avg)/(2*σ))) pop
    where
      fs = map snd pop
      σ   = sqrt . variance $ fs
      f_avg = average fs

-- | Replace fitness values in the population with their ranks.  For a
-- population of size @n@, the best genome has rank @n' <= n@, and the
-- worst genome has rank @1@. 'rankScale' may be useful to avoid
-- domination of few super-genomes in 'rouletteSelect' or to apply
-- 'rouletteSelect' when fitness is not necessarily positive.
rankScale :: Population a -> Population a
rankScale pop =
    let sorted = reverse $ sortByFitness pop
        worstF = snd . head $ sorted
    in  ranks 1 worstF sorted
    where
      ranks _ _ [] = []
      ranks rank worst ((genome,fitness):rest)
          | worst == fitness    = (genome,rank)   : ranks rank worst rest
          | otherwise           = (genome,rank+1) : ranks (rank+1) fitness rest

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
    let winner = head $ eliteGenomes contestants
    return winner

-- | Select @n@ best genomes, then select more genomes from the
-- /entire/ population (elite genomes inclusive). Elite genomes will
-- be the first in the list.
--
-- This function transforms a normal selection operator to selection
-- with elitism. For best results, use 'preserveElite' to transform
-- crossover and mutation operators (TODO).
withElite :: Int -> SelectionOp a -> SelectionOp a
withElite n select = \population -> do
  let elite = take n . eliteGenomes $ population
  selected <- select population
  return (elite ++ selected)

-- | Sort population (a list of (genome,fitness) pairs) by fitness
-- (descending). The best genomes are put in the head of the list.
sortByFitness :: Population a -> Population a
sortByFitness = sortBy (\(_,a) (_,b) -> compare b a)

-- | Takes a list of (genome,fitness) pairs and returns a list of
-- genomes sorted by fitness (descending)
eliteGenomes :: Population a -> [Genome a]
eliteGenomes = map fst . sortByFitness

-- |Modify value with probability @p@.
withProbability :: Double -> a -> (a -> Rand a) -> Rand a
withProbability p x modify = do
  t <- getDouble
  if t < p
     then modify x
     else return x

-- | Crossover two lists in exactly @n@ random points.
nPointCrossover :: Int -> ([a], [a]) -> Rand ([a], [a])
nPointCrossover n (xs,ys)
    | n <= 0 = return (xs,ys)
    | otherwise =
  let len = min (length xs) (length ys)
  in  do
    pos <- getRandomR (0, len-n)
    let (hxs, txs) = splitAt pos xs
    let (hys, tys) = splitAt pos ys
    (rxs, rys) <- nPointCrossover (n-1) (tys, txs) -- FIXME: not tail recursive
    return (hxs ++ rxs, hys ++ rys)

-- |Select a random point in two genomes, and swap them beyond this point.
-- Apply with probability @p@.
onePointCrossover :: Double -> CrossoverOp a
onePointCrossover _ []  = return ([],[])
onePointCrossover _ [_] = error "odd number of parents"
onePointCrossover p (g1:g2:rest) = do
  (h1,h2) <- withProbability p (g1,g2) $ nPointCrossover 1
  return ([h1,h2], rest)

-- |Select two random points in two genomes, ans swap everything in between.
-- Apply with probability @p@.
twoPointCrossover :: Double -> CrossoverOp a
twoPointCrossover _ []  = return ([], [])
twoPointCrossover _ [_] = error "odd number of parents"
twoPointCrossover p (g1:g2:rest) = do
  (h1,h2) <- withProbability p (g1,g2) $ nPointCrossover 2
  return ([h1,h2], rest)

-- |Swap individual bits of two genomes with probability @p@.
uniformCrossover :: Double -> CrossoverOp a
uniformCrossover _ []  = return ([], [])
uniformCrossover _ [_] = error "odd number of parents"
uniformCrossover p (g1:g2:rest) = do
  (h1, h2) <- unzip `liftM` mapM swap (zip g1 g2)
  return ([h1,h2], rest)
  where
    swap (x, y) = withProbability p (x,y) $ \(a,b) -> return (b,a)

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
blendCrossover _ [_] = error "odd number of parents"
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
        (xi:etas) <- getNormals n
        let xi' = sigma_xi * xi
        let parCorr = xi' `scale` d
        let etas' = map (dist3 * sigma_eta *) etas
        let orthCorrs = zipWith scale etas' exs
        return (parCorr, orthCorrs)
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
unimodalCrossover _ _ _  = error "bad number of parents"

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
simulatedBinaryCrossover _ [] = return ([], [])
simulatedBinaryCrossover _ _  = error "bad number of parents"


-- |Flips a random bit along the length of the genome with probability @p@.
-- With probability @(1 - p)@ the genome remains unaffected.
pointMutate :: Double -> MutationOp Bool
pointMutate p bits = withProbability p bits $ \bits -> do
       r <- getRandomR (0, length bits - 1)
       let (before, (bit:after)) = splitAt r bits
       return (before ++ (not bit:after))

-- |For every variable in the genome with probability @p@ replace its
-- value @v@ with @v + sigma*N(0,1)@, where @N(0,1)@ is a normally
-- distributed random variable with mean equal 0 and variance equal 1.
-- With probability @(1 - n*p)@, where @n@ is the number
-- of variables, the genome remains unaffected.
gaussianMutate :: Double -> Double -> MutationOp Double
gaussianMutate p sigma vars = mapM mutate vars
  where
    mutate v  = withProbability p v $ \v -> do
          n <- getNormal
          return (v + sigma*n)

-- |Clip variable @v@ to stay within range @(vmin, vmax)@ (inclusive).
clip :: (Ord a) => (a, a) -> a -> a
clip range v =
    let vmin = uncurry min range
        vmax = uncurry max range
    in  max (min v vmax) vmin

-- |Returns the average fitnesses in a population.
avgFitness :: Population a -> Fitness
avgFitness = average . map snd

-- |Returns the maximum fitness in a population.
maxFitness :: Population a -> Fitness
maxFitness = maximum . map snd

-- |Returns the minimum fitness in a population.
minFitness :: Population a -> Fitness
minFitness = minimum . map snd

-- |Returns the standard deviation of the fitness values in a population.
stdDeviation :: Population a -> Double
stdDeviation = sqrt . variance . map snd

-- Ersatz linear algebra
minus :: Num a => [a] -> [a] -> [a]
minus xs ys  = zipWith (-) xs ys
plus :: Num a => [a] -> [a] -> [a]
plus xs ys   = zipWith (+) xs ys
scale :: Num a => a -> [a] -> [a]
scale a xs   = map (a*) xs
dot :: Num a => [a] -> [a] -> a
dot xs ys    = sum $ zipWith (*) xs ys
norm2 :: (Num a, Floating a) => [a] -> a
norm2 xs     = sqrt $ dot xs xs
proj :: (Num a, Fractional a) => [a] -> [a] -> [a]
proj xs dir  = ( dot xs dir / dot dir dir ) `scale` dir
normalize :: (Num a, Floating a, Fractional a) => [a] -> [a]
normalize xs = let a = norm2 xs in (1.0/a) `scale` xs

-- Ersatz statistics
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

