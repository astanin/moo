{- | Benchmark operators of real-coded genetic algorithms.

Test problem: minimize sphere function.
Usage: @./bench_rcga -o bench_rcga.html@

-}



import Criterion
import Criterion.Main
import Criterion.Config
import Moo.GeneticAlgorithm.Continuous


objective :: [Double] -> Objective
objective xs = sum . map (^2) $ xs


dimensionality = 3
popsize = 8^dimensionality
start = return $ uniformInit popsize (replicate dimensionality (-5.0,5.0))
stopCondition = Generations 100


tournament2 = tournamentSelect Minimizing 2 popsize
rankRoulette = onRank $ rouletteSelect popsize
rankSUS = onRank $ stochasticUniversalSampling popsize
bx = blendCrossover 0.5
undx = unimodalCrossoverRP
sbx = simulatedBinaryCrossover 4.0
gm = gaussianMutate 0.25 0.1
onRank = withPopulationTransform (rankScale Minimizing)


ga select xover mutate = do
  pop <- runGA start $ loop stopCondition $
         nextGeneration Minimizing objective select 0 xover mutate
  let best = head . bestFirst Minimizing $ pop
  print best


main = defaultMainWith defaultConfig { cfgSamples = ljust 10 }
       (return ())
       [ bgroup "crossover"
         [ bench "tournament2-UNDX-GM" $ ga tournament2 undx gm
         , bench "tournament2-blend-GM" $ ga tournament2 bx gm
         , bench "tournament2-SBX0.5-GM" $ ga tournament2 sbx gm
         , bench "tournament2-1point-GM" $ ga tournament2 (onePointCrossover 0.5) gm
         , bench "tournament2-noX-GM" $ ga tournament2 noCrossover gm ]
       , bgroup "selection"
         [ bench "tournament2-noX-GM" $ ga tournament2 noCrossover gm
         , bench "rankRoullette-noX-GM" $ ga rankRoulette noCrossover gm
         , bench "rankSUS-noX-GM" $ ga rankSUS noCrossover gm] ]


-- | Generate at most @popsize@ genomes uniformly distributed in @ranges@.
-- Same as 'uniformGenomes' in moo >= 1.1, not available in moo-1.0.
uniformInit :: Int -> [(Double,Double)] -> [Genome Double]
uniformInit popsize ranges =
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
