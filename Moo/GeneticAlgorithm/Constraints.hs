module Moo.GeneticAlgorithm.Constraints
    ( ConstraintFunction
    , Constraint(..), greaterThan, greaterThanOrEqual
    , satisfiesConstraint
    , isFeasible
    , numberOfViolations
    , degreeOfViolation
    ) where


import Moo.GeneticAlgorithm.Types


type ConstraintFunction a b = Genome a -> b


-- | A constraint.
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


instance (Show b, Num b) => Show (Constraint a b) where
    show (LessThan _ v) = "_ < " ++ show v
    show (LessThanOrEqual _ v) = "_ <= " ++ show v
    show (Equal _ v) = "_ == " ++ show v


-- | An auxiliary constructor for strict inequality constraint.
greaterThan :: (Num b, Show b) => ConstraintFunction a b -> b -> Constraint a b
greaterThan f v = LessThan (negate . f) (negate v)


-- | An auxiliary constructor for non-strict inequality constraint.
greaterThanOrEqual :: (Num b, Show b) => ConstraintFunction a b -> b -> Constraint a b
greaterThanOrEqual f v = LessThanOrEqual (negate . f) (negate v)


-- | Returns @True@ if a @genome@ represents a feasible solution
-- with respect to the @constraint@.
satisfiesConstraint :: (Num b, Ord b)
          => Genome a        -- ^ @genome@
          -> Constraint a b  -- ^ @constraint@
          -> Bool
satisfiesConstraint g (LessThan f v)  = f g < v
satisfiesConstraint g (LessThanOrEqual f v) = f g <= v
satisfiesConstraint g (Equal f v) = f g == v


-- | Returns @True@ if a @genome@ represents a feasible solution,
-- i.e. satisfies all @constraints@.
isFeasible :: (Num b, Ord b)
           => Genome a          -- ^ @genome@
           -> [Constraint a b]  -- ^ @constraints@
           -> Bool
isFeasible genome constraints = all (genome `satisfiesConstraint`) constraints


-- | Count the number of constraint violations. The result can be used
-- as a degree of feasibility to compare unfeasible solutions.
-- Returns @0@ if the solution is feasible.
numberOfViolations :: (Num b, Ord b) => [Constraint a b] -> Genome a -> Int
numberOfViolations constraints genome =
    let satisfied = map (genome `satisfiesConstraint`) constraints
    in  length $ filter not satisfied


-- | A quantitative non-negative estimate of the degree of feasibility.
--
-- Given @f_j@ is an excess of @j@-th constraint function value, return
-- @sum |f_j|^beta@. Return @0.0@ if the solution is feasible.
-- For non-strict inequality constraints, return @sum |f_j + 1.0|^beta@.
--
degreeOfViolation :: Double -> [Constraint a Double] -> Genome a -> Double
degreeOfViolation beta constraints genome =
    sum $ map violation constraints
  where
    violation (LessThan f v) =
        let v' = f genome
        in  if v' < v
            then 0.0
            else (abs $ v' - v) ** beta
    violation (LessThanOrEqual f v) =
        let v' = f genome
        in  if v' <= v
            then 0.0
            else (abs $ 1.0 + v' - v) ** beta
    violation (Equal f v) =
        let v' = f genome
        in  if v' == v
            then 0.0
            else (abs $ v' - v) ** beta
