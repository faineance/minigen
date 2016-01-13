{-# LANGUAGE FlexibleInstances, GADTs #-}

module Optimization where
import Expr


class Optimisable a where
    optimize :: a -> a
    optimize = id

instance Optimisable (Expr t) where
    optimize (Lit v) = Lit v
    optimize (Lit v :+: Lit v') = Lit (v + v')
    optimize (Lit v :*: Lit v') = Lit (v * v')
    optimize (e :*: e') = optimize e :*: optimize e'
    optimize (e :+: e') = optimize e :+: optimize e'




instance Optimisable [Instr] where
    optimize (Push r : Pop r' : xs) | r == r' = optimize xs
    optimize (Set r v : x'@(Set r' v') : xs ) = optimize (x' : xs)
    optimize (Set r v : xs ) = optimize (Xor r (Left r) : Or r (Right v): xs)

    optimize (x : xs) = x : optimize xs
    optimize [] = []
