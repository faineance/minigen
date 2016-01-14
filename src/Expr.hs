{-# LANGUAGE GADTs     #-}
module Expr  where

-- Type-level naturals
data Zero
data Succ a

-- Input lang
data Expr t  where
    Lit :: Int -> Expr Int
    App :: Expr t ->  Expr t
    (:+:) :: Expr t -> Expr t -> Expr t
    (:*:) :: Expr t -> Expr t -> Expr t



instance Show (Expr t) where
    show (Lit i) = show i
    show ((:+:) e e') = show e ++ " + " ++ show e'
    show ((:*:) e e') = show e ++ " + " ++ show e'
