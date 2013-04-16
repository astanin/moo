module Moo.GeneticAlgorithm.Constraints
    (
      ConstraintFunction
    , Constraint()
    , (.<.), (.<=.), (.>.), (.>=.), (.==.), (.<=..<=.), (.<..<.)
    , isFeasible
    -- ** Constrained initalization
    , getConstrainedGenomesRs
    -- ** Constrained selection
    , withDeathPenalty
    , withFinalDeathPenalty
    , withConstraints
    , constrainedTournament
    , numberOfViolations
    , degreeOfViolation
    ) where


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Utilities (getRandomGenomesRs)
import Moo.GeneticAlgorithm.Selection (withPopulationTransform, bestFirst)


import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.List (sortBy)
import Data.Function (on)


type ConstraintFunction a b = Genome a -> b


-- Defining a constraint as a pair of function and its boundary value
-- (vs just a boolean valued function) allows for estimating the
-- degree of constraint violation when necessary.

-- | Define constraints using '.<.', '.<=.', '.>.', '.>=.', and '.==.'
-- operators, with 'ConstraintFunction' on the left hand side.
-- For double inequality constraints use '.<=..<=.' and '.<..<.'.
data (Num b) => Constraint a b
    = LessThan (ConstraintFunction a b) b
    -- ^ strict inequality constraint,
    -- function value is less than the constraint value
    | LessThanOrEqual (ConstraintFunction a b) b
    -- ^ non-strict inequality constraint,
    -- function value is less than or equal to the constraint value
    | Equal (ConstraintFunction a b) b
    -- ^ equality constraint,
    -- function value is equal to the constraint value
    | InInterval (ConstraintFunction a b) (Bool, b) (Bool, b)
    -- ^ double inequality, boolean flags indicate if the
    -- bound is inclusive.


(.<.) :: (Num b) => ConstraintFunction a b -> b -> Constraint a b
(.<.) = LessThan

(.<=.) :: (Num b) => ConstraintFunction a b -> b -> Constraint a b
(.<=.) = LessThanOrEqual

(.>.) :: (Num b) => ConstraintFunction a b -> b -> Constraint a b
(.>.) f v = LessThan (negate . f) (negate v)

(.>=.) :: (Num b) => ConstraintFunction a b -> b -> Constraint a b
(.>=.) f v = LessThanOrEqual (negate . f) (negate v)

(.==.) :: (Num b) => ConstraintFunction a b -> b -> Constraint a b
(.==.) = Equal

(.<=..<=.) :: (Num b) => b -> b -> ((ConstraintFunction a b) -> Constraint a b)
l .<=..<=. r = \cf -> InInterval cf (True, l) (True, r)

(.<..<.) :: (Num b) => b -> b -> ((ConstraintFunction a b) -> Constraint a b)
l .<..<. r = \cf -> InInterval cf (False, l) (False, r)


-- | Returns @True@ if a @genome@ represents a feasible solution
-- with respect to the @constraint@.
satisfiesConstraint :: (Real b)
          => Genome a        -- ^ @genome@
          -> Constraint a b  -- ^ @constraint@
          -> Bool
satisfiesConstraint g (LessThan f v)  = f g < v
satisfiesConstraint g (LessThanOrEqual f v) = f g <= v
satisfiesConstraint g (Equal f v) = f g == v
satisfiesConstraint g (InInterval f (inclusive1,v1) (inclusive2,v2)) =
    let v' = f g
        c1 = if inclusive1 then v1 <= v' else v1 < v'
        c2 = if inclusive2 then v' <= v2 else v' < v2
    in  c1 && c2



-- | Returns @True@ if a @genome@ represents a feasible solution,
-- i.e. satisfies all @constraints@.
isFeasible :: (Real b)
           => [Constraint a b]  -- ^ constraints
           -> Genome a          -- ^ genome
           -> Bool
isFeasible constraints genome = all (genome `satisfiesConstraint`) constraints


-- | Generate @n@ feasible random genomes with individual genome elements
-- bounded by @ranges@.
getConstrainedGenomesRs :: (Random a, Ord a, Real b)
    => [Constraint a b]   -- ^ constraints
    -> Int                -- ^ @n@, how many genomes to generate
    -> [(a, a)]           -- ^ ranges for individual genome elements
    -> Rand ([Genome a])  -- ^ random feasible genomes
getConstrainedGenomesRs constraints n ranges
  | n <= 0            = return []
  | otherwise         = do
  candidates <- getRandomGenomesRs n ranges
  let feasible = filter (isFeasible constraints) candidates
  let found = length feasible
  more <- getConstrainedGenomesRs constraints (n - found) ranges
  return $ feasible ++ more


-- | A simple estimate of the degree of (in)feasibility.
--
-- Count the number of constraint violations. Return @0@ if the solution is feasible.
numberOfViolations :: (Real b)
                   => [Constraint a b]  -- ^ constraints
                   -> Genome a  -- ^ genome
                   -> Int  -- ^ the number of violated constraints
numberOfViolations constraints genome =
    let satisfied = map (genome `satisfiesConstraint`) constraints
    in  length $ filter not satisfied


-- | An estimate of the degree of (in)feasibility.
--
-- Given @f_j@ is the excess of @j@-th constraint function value,
-- return @sum |f_j|^beta@.  For strict inequality constraints, return
-- @sum (|f_j|^beta + eta)@.  Return @0.0@ if the solution is
-- feasible.
--
degreeOfViolation :: Double  -- ^ beta, single violation exponent
                  -> Double  -- ^ eta, equality penalty in strict inequalities
                  -> [Constraint a Double] -- ^ constrains
                  -> Genome a  -- ^ genome
                  -> Double    -- ^ total degree of violation
degreeOfViolation beta eta constraints genome =
    sum $ map violation constraints
  where
    violation (LessThan f v) =
        let v' = f genome
        in  if v' < v
            then 0.0
            else (abs $ v' - v) ** beta + eta
    violation (LessThanOrEqual f v) =
        let v' = f genome
        in  if v' <= v
            then 0.0
            else (abs $ v' - v) ** beta
    violation (Equal f v) =
        let v' = f genome
        in  if v' == v
            then 0.0
            else (abs $ v' - v) ** beta
    violation (InInterval f (incleft, l) (incright, r)) =
        let v' = f genome
            leftok = if incleft
                     then l <= v'
                     else l < v'
            rightok = if incright
                      then r <= v'
                      else r < v'
        in  case (leftok, rightok) of
            (True, True) -> 0.0
            (False, _)   -> (abs $ l - v') ** beta
                            + (fromIntegral . fromEnum . not $ incleft) * eta
            (_, False)   -> (abs $ v' - r) ** beta
                            + (fromIntegral . fromEnum . not $ incright) * eta


-- | Constrained tournament selection prefers feasible solutions over infeasible,
-- solutions with small constraint violations over other infeasible solutions,
-- and is equivalent to normal tournament if all solutions are feasible.
-- Runs @n@ tournaments in groups of size @size@.
--
-- 'constrainedTournament' is inspired by the ideas of the constrained
-- tournament selection operator of (Deb, 2000), but differs in
-- implementation, and allows for an arbitrary measure of the
-- constraint violation ((Deb, 2000) used a sum of absolute values,
-- use 'degreeOfViolation' @1.0 0.0@ for the same effect).
--
-- Note: 'constrainedTournament' runs in the beginning of every iteration,
-- more infeasible solutions may appear after crossover and mutation.
-- Consider taking only feasible solutions after the last iteration
-- (see 'isFeasible' and 'withFinalDeathPenalty').
--
-- Reference: Deb, K. (2000). An efficient constraint handling method
-- for genetic algorithms. Computer methods in applied mechanics and
-- engineering, 186(2), 311-338.
constrainedTournament :: (Real b, Real c)
    => [Constraint a b]  -- ^ constraints
    -> ([Constraint a b] -> Genome a -> c)  -- ^ degree of violation function,
                                            -- see 'numberOfViolations' and 'degreeOfViolation'
    -> ProblemType  -- ^ type of the optimization problem
    -> Int -- ^ @size@ of the tournament group
    -> Int -- ^ @n@, how many tournaments to run
    -> SelectionOp a
constrainedTournament constraints violation ptype size n xs =
    replicateM n tournament1
  where
  xs_ds =
      let ds = map (violation constraints . takeGenome) xs
      in  zip xs ds
  cmp = (constrainedCompare ptype) `on` (first takeObjectiveValue)
  tournament1 = do
    contestants <- randomSample size xs_ds
    let winner = fst . head $ sortBy cmp contestants
    return winner

-- | Modify objective function in such a way that 1) any feasible
-- solution is preferred to any infeasible solution, 2) among two
-- feasible solutions the one having better objective function value
-- is preferred, 3) among two infeasible solution the one having
-- smaller constraint violation is preferred.
--
-- Reference: Deb, K. (2000). An efficient constraint handling method
-- for genetic algorithms. Computer methods in applied mechanics and
-- engineering, 186(2), 311-338.
withConstraints :: (Real b, Real c)
    => [Constraint a b]                      -- ^ constraints
    -> ([Constraint a b] -> Genome a -> c)   -- ^ non-negative degree of violation,
                                             -- see 'numberOfViolations' and 'degreeOfViolation'
    -> ProblemType
    -> SelectionOp a
    -> SelectionOp a
withConstraints constraints violation ptype =
    withPopulationTransform penalizeInfeasible
  where
    penalizeInfeasible phenotypes =
        let worst = takeObjectiveValue . head . worstFirst ptype $ phenotypes
            penalize p = let g = takeGenome p
                             v = fromRational . toRational . violation constraints $ g
                         in  if (v > 0)
                             then (g, worst `worsen` v)
                             else p
        in  map penalize phenotypes

    worstFirst Minimizing = bestFirst Maximizing
    worstFirst Maximizing = bestFirst Minimizing

    worsen x delta = if ptype == Minimizing
                     then x + delta
                     else x - delta

constrainedCompare :: (Real c) => ProblemType -> (Objective, c) -> (Objective, c) -> Ordering
constrainedCompare ptype (objval1, violation1) (objval2, violation2) =
    let infeasible1 = violation1 > 0
        infeasible2 = violation2 > 0
    in  case (infeasible1, infeasible2) of
          (True, True) -> compare violation1 violation2  -- prefer smaller violation
          (False, False) -> case ptype of
                              Minimizing -> compare objval1 objval2
                              Maximizing -> compare objval2 objval1
          (True, False) -> GT  -- second (feasible) solution is preferred
          (False, True) -> LT  -- first (feasible) solution is preferred


-- | Kill all infeasible solutions after every step of the genetic algorithm.
--
-- “Death penalty is very popular within the evolution strategies community,
-- but it is limited to problems in which the feasible search space is convex
-- and constitutes a reasonably large portion of the whole search space,” --
-- (Coello 1999).
--
-- Coello, C. A. C., & Carlos, A. (1999). A survey of constraint
-- handling techniques used with evolutionary algorithms.
-- Lania-RI-99-04, Laboratorio Nacional de Informática Avanzada.
withDeathPenalty :: (Monad m, Real b)
                 => [Constraint a b]  -- ^ constraints
                 -> StepGA m a        -- ^ unconstrained step
                 -> StepGA m a        -- ^ constrained step
withDeathPenalty cs step =
    \stop popstate -> do
      stepresult <- step stop popstate
      case stepresult of
        StopGA pop -> return (StopGA (filterFeasible cs pop))
        ContinueGA pop -> return (ContinueGA (filterFeasible cs pop))


-- | Kill all infeasible solutions once after the last step of the
-- genetic algorithm. See also 'withDeathPenalty'.
withFinalDeathPenalty :: (Monad m, Real b)
                      => [Constraint a b]  -- ^ constriants
                      -> StepGA m a        -- ^ unconstrained step
                      -> StepGA m a        -- ^ constrained step
withFinalDeathPenalty cs step =
    \stop popstate -> do
      result <- step stop popstate
      case result of
        (ContinueGA _) -> return result
        (StopGA pop) -> return (StopGA (filterFeasible cs pop))


filterFeasible :: (Real b) => [Constraint a b] -> Population a -> Population a
filterFeasible cs = filter (isFeasible cs . takeGenome)
