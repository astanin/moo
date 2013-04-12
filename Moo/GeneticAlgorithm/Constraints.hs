module Moo.GeneticAlgorithm.Constraints
    ( ConstraintFunction
    , Constraint()
    , lessThan, lessThanOrEqual
    , greaterThan, greaterThanOrEqual, equal
    , isFeasible
    , numberOfViolations
    , degreeOfViolation
    , constrainedTournament
    , withDeathPenalty
    ) where


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random


import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.List (sortBy)
import Data.Function (on)


type ConstraintFunction a b = Genome a -> b


-- A constraint.
--
-- Defining a constraint as a pair of function and its boundary value
-- (vs just a boolean valued function) allows for estimating the
-- degree of constraint violation when necessary.
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


-- | Strict inequality
lessThan :: (Num b, Show b) => ConstraintFunction a b -> b -> Constraint a b
lessThan = LessThan

-- | Non-strict inequality
lessThanOrEqual :: (Num b, Show b) => ConstraintFunction a b -> b -> Constraint a b
lessThanOrEqual = LessThanOrEqual

-- | Strict inequality
greaterThan :: (Num b, Show b) => ConstraintFunction a b -> b -> Constraint a b
greaterThan f v = LessThan (negate . f) (negate v)

-- | Non-strict inequality
greaterThanOrEqual :: (Num b, Show b) => ConstraintFunction a b -> b -> Constraint a b
greaterThanOrEqual f v = LessThanOrEqual (negate . f) (negate v)

-- | Strict equality
equal :: (Num b, Show b) => ConstraintFunction a b -> b -> Constraint a b
equal = Equal


-- | Returns @True@ if a @genome@ represents a feasible solution
-- with respect to the @constraint@.
satisfiesConstraint :: (Real b)
          => Genome a        -- ^ @genome@
          -> Constraint a b  -- ^ @constraint@
          -> Bool
satisfiesConstraint g (LessThan f v)  = f g < v
satisfiesConstraint g (LessThanOrEqual f v) = f g <= v
satisfiesConstraint g (Equal f v) = f g == v


-- | Returns @True@ if a @genome@ represents a feasible solution,
-- i.e. satisfies all @constraints@.
isFeasible :: (Real b)
           => Genome a          -- ^ genome
           -> [Constraint a b]  -- ^ constraints
           -> Bool
isFeasible genome constraints = all (genome `satisfiesConstraint`) constraints


-- | A simple estimate of the degree of feasibility.
--
-- Count the number of constraint violations. Return @0@ if the solution is feasible.
numberOfViolations :: (Real b)
                   => [Constraint a b]  -- ^ constraints
                   -> Genome a  -- ^ genome
                   -> Int  -- ^ the number of violated constraints
numberOfViolations constraints genome =
    let satisfied = map (genome `satisfiesConstraint`) constraints
    in  length $ filter not satisfied


-- | An estimate of the degree of feasibility.
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


-- | Constrained tournament selection prefers feasible solutions over infeasible,
-- solutions with small constraint violations over other infeasible solutions,
-- and is equivalent to normal tournament if all solutions are feasible.
-- Runs @n@ tournaments in groups of size @size@.
constrainedTournament :: (Real b, Real c)
    => [Constraint a b]  -- ^ constraints
    -> ([Constraint a b] -> Genome a -> c)  -- ^ degree of violation function
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
withDeathPenalty :: (Monad m, Real b)
                 => [Constraint a b]  -- ^ constraints
                 -> StepGA m a        -- ^ unconstrained step
                 -> StepGA m a        -- ^ constrained step
withDeathPenalty cs step =
    \stop popstate -> do
      stepresult <- step stop popstate
      case stepresult of
        StopGA pop -> return (StopGA (killInfeasible pop))
        ContinueGA pop -> return (ContinueGA (killInfeasible pop))
  where
    killInfeasible = filter (\p -> (takeGenome p) `isFeasible` cs)