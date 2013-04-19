-- | The boring part common to many examples: command line options
-- and pretty-printing the results.
module ExampleMain
    ( exampleMain
    , ExampleDefaults(..)
    , exampleDefaults
    ) where


import Moo.GeneticAlgorithm.Binary
import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Multiobjective
import Moo.GeneticAlgorithm.Statistics


import Control.Monad (liftM, when)
import Data.List (intercalate)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import Text.Printf


data Flag = RunGenerations Int
          | PrintBest Bool
          | PrintStats Bool
          | DumpAll Bool
          | ShowHelp
            deriving (Show, Eq)


data ExampleDefaults = ExampleDefaults
    { numGenerations :: Int
    , printBest :: Bool
    , printStats :: Bool
    , dumpAll :: Bool
    } deriving (Show, Eq)


exampleDefaults = ExampleDefaults {
                    numGenerations = 100
                  , printBest = True
                  , printStats = False
                  , dumpAll = False
                  }


exampleOptions :: ExampleDefaults -> [OptDescr Flag]
exampleOptions c =
    [ Option "gn" ["generations"]
                 (ReqArg (RunGenerations . read) "N")
                 ("number of generations (default: " ++ show (numGenerations c) ++ ")")
    , Option "b" ["best"]
                 (NoArg $ PrintBest True)
                 ("print the best solution" ++ (isDefault (printBest c)))
    , Option ""  ["no-best"]
                 (NoArg $ PrintBest False)
                 ("don't print the best solution" ++ (isDefault (not . printBest $ c)))
    , Option "d" ["dump"]
                 (NoArg $ DumpAll True)
                 ("dump the entire population and its objective values" ++ isDefault (dumpAll c))
    , Option ""  ["no-dump"]
                 (NoArg $ DumpAll False)
                 ("don't dump the entire population" ++ isDefault (not . dumpAll $ c))
    , Option "s" ["stats"]
                 (NoArg $ PrintStats True)
                 ("print population statistics" ++ isDefault (printStats c))
    , Option ""  ["no-stats"]
                 (NoArg $ PrintStats False)
                 ("don't print population statistics" ++ isDefault (not . printStats $ c))
    , Option "h" ["help"]
                 (NoArg ShowHelp)
                 "show help"
    ]
   where
   isDefault :: Bool -> String
   isDefault True = " (default)"
   isDefault False = ""


updateDefaults :: ExampleDefaults -> [Flag] -> ExampleDefaults
updateDefaults d (RunGenerations n:opts) = updateDefaults (d { numGenerations = n }) opts
updateDefaults d (PrintBest b:opts) = updateDefaults (d { printBest = b }) opts
-- --stats overrid --dump, and vice versa
updateDefaults d (DumpAll b:opts) =
    let ps = printStats d
    in  flip updateDefaults opts (d { dumpAll = b, printStats = ps && (not b)})
updateDefaults d (PrintStats b:opts) =
    let da = dumpAll d
    in  flip updateDefaults opts (d { printStats = b, dumpAll = da && (not b)})
updateDefaults d [] = d



printHeader conf = do
  when (printStats conf) $ putStrLn "# best, median"
  when (dumpAll conf) $ putStrLn "# x1, x2, ..., objective1, objective2, ..."


printSnapshot conf sorted = do
  when (printBest conf) $
    if null sorted
       then putStrLn "# no solutions"
       else putStrLn $ "# best found: " ++ fmtPt (head sorted)

  when (printStats conf) $ do
    printHeader conf
    let ovs = map takeObjectiveValue sorted
    let obest = head ovs
    let omedian = median ovs
    putStrLn $ fmtXs " " [obest, omedian]

  when (dumpAll conf) $ do
    printHeader conf
    -- print the best solution last;
    -- (for scatter-plotting it above the others)
    flip mapM_ (reverse sorted) $ \p -> putStrLn $ fmtPtOneline p
    putStrLn ""

  where

    fmtPt :: (Show a, Real a, PrintfArg a) => Phenotype a -> String
    fmtPt (xs, v) = (printf "%.3g @ [" v) ++ fmtXs ", " xs ++ "]"

    fmtPtOneline :: (Show a, Real a, PrintfArg a) => Phenotype a -> String
    fmtPtOneline p = let xs = map (fromRational.toRational) . takeGenome $ p
                         vs = [takeObjectiveValue p]
                     in  fmtXs " " $ xs ++ vs

    fmtXs :: (Show a, Real a, PrintfArg a) => String -> [a] -> String
    fmtXs sep xs =  intercalate sep $ map (printf "%.3g") xs



-- | Run a genetic algorithm defined by @problemtype@, and @step@.
-- Process command line options to change the number of iterations
-- and logging behaviour.
exampleMain :: (Show a, Real a, PrintfArg a)
            => ExampleDefaults -> ProblemType -> Rand [Genome a] -> StepGA Rand a -> IO ()
exampleMain defaults problemtype initialize step = do

  let options = exampleOptions defaults
  (opts, args, msgs) <- liftM (getOpt Permute options) getArgs
  when (ShowHelp `elem` opts) $ do
    progname <- getProgName
    let header = "usage: " ++ progname ++ " [options]\n\nOptions:\n"
    putStrLn (usageInfo header options)
    exitSuccess

  let conf = updateDefaults defaults opts
  let gens = numGenerations conf
  result <- runGA initialize (loop (Generations gens) step)
  let sorted = bestFirst problemtype $ result
  printSnapshot conf sorted
