{- | Benchmark operators of real-coded genetic algorithms.

Test problem: minimize sphere function.
Usage: @./bench_rcga -o bench_rcga.html@

-}



import Criterion
import Criterion.Main
import Criterion.Config
import Moo.GeneticAlgorithm.Continuous
import Data.Function (on)


fitness :: [Double] -> Objective
fitness xs = let r2 = sum . map (^2) $ xs
               in  1.0 / (1.0 + r2)


dimensionality = 3
popsize = 7^dimensionality
generations = 100
population0 =
    let genomes = uniformInit popsize (replicate dimensionality (-5.0,5.0))
    in  evalObjective fitness genomes
genomes0 =
    map takeGenome population0
start = return genomes0
stopCondition = Generations generations


tournament2 = tournamentSelect Maximizing 2 popsize
rankRoulette = onRank $ rouletteSelect popsize
rankSUS = onRank $ stochasticUniversalSampling popsize
roulette = rouletteSelect popsize
sus = stochasticUniversalSampling popsize
bx = blendCrossover 0.5
undx = unimodalCrossoverRP
sbx = simulatedBinaryCrossover 4.0
p1x = onePointCrossover 0.5
gm = gaussianMutate 0.25 0.1
onRank = withPopulationTransform (rankScale Maximizing)
withNiching = withFitnessSharing (distance2 `on` takeGenome) 0.01 1 Maximizing



-- run the entire GA normally
ga select xover mutate = do
  pop <- runGA start $ loop stopCondition $
         nextGeneration Maximizing fitness select 0 xover mutate
  let best = head . bestFirst Maximizing $ pop
  print best


-- apply selection operator the same number of times as in normal GA run
runSelection select = do
  rng <- newPureMT
  let pop' = flip evalRandom rng $ doTimes generations select population0
  putStr . (++" ") . show . minimum . map takeObjectiveValue $ pop'


-- apply crossover operator the same number of times as in normal GA run
runCrossover crossover = do
  rng <- newPureMT
  let genomes' = flip evalRandom rng $
                 doTimes generations
                 (flip doCrossovers crossover) genomes0
  putStr . (++" ") . show . minimum . concat $ genomes'


-- apply mutation operator the same number of times as in normal GA run
runMutation mutate = do
  rng <- newPureMT
  let genomes' = flip evalRandom rng $
                 doTimes generations (mapM gm) genomes0
  putStr . (++" ") . show . minimum . concat $ genomes'


doTimes 0 _ p = return p
doTimes n action p = action p >>= doTimes (n-1) action


main = defaultMainWith defaultConfig { cfgSamples = ljust 20 }
       (return ())
       [ bgroup "synthetic"
         [ bench "tournament2-UNDX-GM" $ ga tournament2 undx gm
         , bench "tournament2-blend-GM" $ ga tournament2 bx gm
         , bench "tournament2-SBX0.5-GM" $ ga tournament2 sbx gm
         , bench "tournament2-1point-GM" $ ga tournament2 p1x gm
         , bench "SUS(rank)-1point-GM" $ ga rankSUS p1x gm
         , bench "SUS-1point-GM" $ ga sus p1x gm
         ]
       , bgroup "selection"
         [ bench "rank-roulette" $ runSelection rankRoulette
         , bench "roulette" $ runSelection roulette
         , bench "tournament2" $ runSelection tournament2
         , bench "rank-SUS" $ runSelection rankSUS
         , bench "SUS" $ runSelection sus
         ]
       , bgroup "crossover"
         [ bench "UNDX" $ runCrossover undx
         , bench "blend" $ runCrossover bx
         , bench "SBX-0.5" $ runCrossover sbx
         , bench "uniform" $ runCrossover (uniformCrossover 0.5)
         , bench "1-point" $ runCrossover p1x
         , bench "noCrossover" $ runCrossover noCrossover
         ]
       , bgroup "mutation"
         [ bench "gaussian" $ runMutation gm
         ] ]


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
