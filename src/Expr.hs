{-# LANGUAGE GADTs     #-}
module Expr  where

-- Type-level naturals
data Zero
data Succ a

data Register = RAX | RBX deriving (Show, Eq)

-- Target machine
type Program = [Instr]
data Instr = Push Register
        | Pop Register
        | Set Register Int
        | Add Register Register
        | Mul Register Register
        deriving Show

-- Input lang
data Expr t where
    Lit :: Int -> Expr Int
    (:+:) :: Expr Int -> Expr Int -> Expr Int
    (:*:) :: Expr Int -> Expr Int -> Expr Int


instance Show (Expr t) where
    show (Lit i) = show i
    show ((:+:) e e') = show e ++ " + " ++ show e'
    show ((:*:) e e') = show e ++ " + " ++ show e'
