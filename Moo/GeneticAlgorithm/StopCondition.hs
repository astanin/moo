module Moo.GeneticAlgorithm.StopCondition where


import Moo.GeneticAlgorithm.Types


evalCond :: (Cond a) -> Population a -> Bool
evalCond (Generations n) _  = n <= 0
evalCond (IfObjective cond) p = cond . map takeObjectiveValue $ p
evalCond (GensNoChange n _ Nothing) _ = n <= 1
evalCond (GensNoChange n f (Just (prev, count))) p =
    let new = f . map takeObjectiveValue $ p
    in  (new == prev) && (count + 1 > n)
evalCond (Or c1 c2) x = evalCond c1 x || evalCond c2 x
evalCond (And c1 c2) x = evalCond c1 x && evalCond c2 x


updateCond :: Population a -> Cond a -> Cond a
updateCond _ (Generations n) = Generations (n-1)
updateCond p (GensNoChange n f Nothing) =
     -- called 1st time _after_ the 1st iteration
    let counter = (Just (f (map takeObjectiveValue p), 1))
    in GensNoChange n f counter
updateCond p (GensNoChange n f (Just (v, c))) =
    let v' = f (map takeObjectiveValue p) in if v' == v
       then GensNoChange n f (Just (v, c+1))
       else GensNoChange n f (Just (v', 1))
updateCond p (And c1 c2) = And (updateCond p c1) (updateCond p c2)
updateCond p (Or c1 c2) = Or (updateCond p c1) (updateCond p c2)
updateCond _ c = c
