{-# LANGUAGE FlexibleInstances, GADTs #-}


module Optimization where
import Expr


class Optimisable a where
    optimize :: a -> a
    optimize = id

instance {-# OVERLAPPABLE #-} (Optimisable a, Functor f) => Optimisable (f a) where
    optimize = fmap optimize

instance Optimisable (Expr t) where
    optimize (Lit v) = Lit v
    optimize (Lit v :+: Lit v') = Lit (v + v')
    optimize (Lit v :*: Lit v') = Lit (v * v')
    optimize (e :*: e') = optimize e :*: optimize e'
    optimize (e :+: e') = optimize e :+: optimize e'

instance Optimisable [Instr] where
    optimize (Push r : Pop r' : xs) | r == r' = xs
    optimize (Set r v : x'@(Set r' v') : xs ) = x' : xs
    optimize (x : xs) = x : optimize xs
