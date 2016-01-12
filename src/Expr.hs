{-# LANGUAGE GADTs     #-}
module Expr  where

-- Type-level naturals
data Zero
data Succ a

data Register = EAX | EBX deriving (Show, Eq)

-- Target machine
type Program = [Instr]
data Instr = Push Register
        | Pop Register
        | Set Register Int
        | Add Register Register
        | Mul Register Register
        deriving Show

-- Input lang
data Expr t  where
    Lit :: Int -> Expr Int
    -- App :: Expr t -> Expr t
    (:+:) :: Expr t -> Expr t -> Expr t
    (:*:) :: Expr t -> Expr t -> Expr t

eval :: (Num t) => Expr t -> t
eval (Lit v) = v
eval (e :+: e') = eval e + eval e'
eval (e :*: e') = eval e * eval e'

instance Show (Expr t) where
    show (Lit i) = show i
    show ((:+:) e e') = show e ++ " + " ++ show e'
    show ((:*:) e e') = show e ++ " + " ++ show e'
