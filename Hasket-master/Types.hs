module Types (LocalEnvironment, Environment, AllEnvironments, Evaluated, Primitive, ImportEvaluated) where

type LocalEnvironment = [(String, String)]
type Environment = [LocalEnvironment]
type AllEnvironments = (Int,[(Int, Environment)])
type Evaluated = Either String (String, Environment)
type Primitive = Environment -> [String] -> Evaluated
type ImportEvaluated = Either Evaluated [Evaluated]