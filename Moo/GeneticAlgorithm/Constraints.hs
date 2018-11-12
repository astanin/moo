module Moo.GeneticAlgorithm.Constraints
    (
      ConstraintFunction
    , Constraint()
    , isFeasible
    -- *** Simple equalities and inequalities
    , (.<.), (.<=.), (.>.), (.>=.), (.==.)
    -- *** Double inequalities
    , LeftHandSideInequality()
    , (.<), (.<=), (<.), (<=.)
    -- ** Constrained initalization
    , getConstrainedGenomes
    , getConstrainedBinaryGenomes
    -- ** Constrained selection
    , withDeathPenalty
    , withFinalDeathPenalty
    , withConstraints
    , numberOfViolations
    , degreeOfViolation
    ) where


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Utilities (getRandomGenomes)
import Moo.GeneticAlgorithm.Selection (withPopulationTransform, bestFirst)


type ConstraintFunction a b = Genome a -> b


-- Defining a constraint as a pair of function and its boundary value
-- (vs just a boolean valued function) allows for estimating the
-- degree of constraint violation when necessary.

-- | Define constraints using '.<.', '.<=.', '.>.', '.>=.', and '.==.'
-- operators, with a 'ConstraintFunction' on the left hand side.
--
-- For double inequality constraints use pairs of '.<', '<.' and
-- '.<=', '<=.' respectively, with a 'ConstraintFunction' in the middle.
--
-- Examples:
--
-- @
-- function .>=. lowerBound
-- lowerBound .<= function <=. upperBound
-- @
data Constraint a b
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


(.<.) :: (Real b) => ConstraintFunction a b -> b -> Constraint a b
(.<.) = LessThan

(.<=.) :: (Real b) => ConstraintFunction a b -> b -> Constraint a b
(.<=.) = LessThanOrEqual

(.>.) :: (Real b) => ConstraintFunction a b -> b -> Constraint a b
(.>.) f v = LessThan (negate . f) (negate v)

(.>=.) :: (Real b) => ConstraintFunction a b -> b -> Constraint a b
(.>=.) f v = LessThanOrEqual (negate . f) (negate v)

(.==.) :: (Real b) => ConstraintFunction a b -> b -> Constraint a b
(.==.) = Equal


-- Left hand side of the double inequality defined in the form:
-- @lowerBound .<= function <=. upperBound@.
data LeftHandSideInequality a b
    = LeftHandSideInequality (ConstraintFunction a b) (Bool, b)
    -- ^ boolean flag indicates if the bound is inclusive

(.<=) :: (Real b) => b -> ConstraintFunction a b -> LeftHandSideInequality a b
lval .<= f = LeftHandSideInequality f (True, lval)

(.<) :: (Real b) => b -> ConstraintFunction a b -> LeftHandSideInequality a b
lval .< f  = LeftHandSideInequality f (False, lval)

(<.) :: (Real b) => LeftHandSideInequality a b -> b -> Constraint a b
(LeftHandSideInequality f l) <. rval  = InInterval f l (False, rval)

(<=.) :: (Real b) => LeftHandSideInequality a b -> b -> Constraint a b
(LeftHandSideInequality f l) <=. rval = InInterval f l (True,  rval)



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
isFeasible :: (GenomeState gt a, Real b)
           => [Constraint a b]  -- ^ constraints
           -> gt                -- ^ genome
           -> Bool
isFeasible constraints genome = all ((takeGenome genome) `satisfiesConstraint`) constraints


-- | Generate @n@ feasible random genomes with individual genome elements
-- bounded by @ranges@.
getConstrainedGenomes :: (Random a, Ord a, Real b)
    => [Constraint a b]   -- ^ constraints
    -> Int                -- ^ @n@, how many genomes to generate
    -> [(a, a)]           -- ^ ranges for individual genome elements
    -> Rand ([Genome a])  -- ^ random feasible genomes
getConstrainedGenomes constraints n ranges
  | n <= 0            = return []
  | otherwise         = do
  candidates <- getRandomGenomes n ranges
  let feasible = filter (isFeasible constraints) candidates
  let found = length feasible
  more <- getConstrainedGenomes constraints (n - found) ranges
  return $ feasible ++ more


-- | Generate @n@ feasible random binary genomes.
getConstrainedBinaryGenomes :: (Real b)
    => [Constraint Bool b]  -- ^ constraints
    -> Int                  -- ^ @n@, how many genomes to generate
    -> Int                  -- ^ @L@, genome length
    -> Rand [Genome Bool]   -- ^ random feasible genomes
getConstrainedBinaryGenomes constraints n len =
    getConstrainedGenomes constraints n (replicate len (False,True))


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
                      then r >= v'
                      else r > v'
        in  case (leftok, rightok) of
            (True, True) -> 0.0
            (False, _)   -> (abs $ l - v') ** beta
                            + (fromIntegral . fromEnum . not $ incleft) * eta
            (_, False)   -> (abs $ v' - r) ** beta
                            + (fromIntegral . fromEnum . not $ incright) * eta


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
    withPopulationTransform (penalizeInfeasible constraints violation ptype)


penalizeInfeasible :: (Real b, Real c)
    => [Constraint a b]
    -> ([Constraint a b] -> Genome a -> c)
    -> ProblemType
    -> Population a
    -> Population a
penalizeInfeasible constraints violation ptype phenotypes =
        let worst = takeObjectiveValue . head . worstFirst ptype $ phenotypes
            penalize p = let g = takeGenome p
                             v = fromRational . toRational . violation constraints $ g
                         in  if (v > 0)
                             then (g, worst `worsen` v)
                             else p
        in  map penalize phenotypes
   where
    worstFirst Minimizing = bestFirst Maximizing
    worstFirst Maximizing = bestFirst Minimizing

    worsen x delta = if ptype == Minimizing
                     then x + delta
                     else x - delta


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
