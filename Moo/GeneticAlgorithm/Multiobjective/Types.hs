module Moo.GeneticAlgorithm.Multiobjective.Types
    ( SingleObjectiveProblem
    , MultiObjectiveProblem
    ) where


import Moo.GeneticAlgorithm.Types


type SingleObjectiveProblem fn = ( ProblemType , fn )
type MultiObjectiveProblem fn = [SingleObjectiveProblem fn]
