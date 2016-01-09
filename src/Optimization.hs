{-# LANGUAGE FlexibleInstances, GADTs, OverlappingInstances #-}


module Optimization where
import Expr


class Optimisable a where
    optimize :: a -> a
    optimize = id

instance (Optimisable a, Functor f) => Optimisable (f a) where
    optimize = fmap optimize

instance Optimisable (Expr t) where
    optimize (Lit v) = Lit v
    optimize (e :*: e') = e :*: e'
    optimize (e :+: e') = e :+: e'

instance Optimisable [Instr] where
    optimize (Push r : Pop r' : xs) | r == r' = xs
    optimize (Set r v : x'@(Set r' v') : xs ) = x' : xs
    optimize (x : xs) = x : optimize xs



-- type Optimization = Program -> Writer [String] Program

-- optimize :: Program
-- optimize program = fst $ runWriter $ foldM (\program f -> f program) program [ reduce ]
--
--
--
-- reduce :: Optimization
-- reduce (Push r : Pop r' : xs) | r == r' = return xs
-- reduce (Set r v : x'@(Set r' v') : xs ) = return (x' : xs)
-- reduce program = return program
