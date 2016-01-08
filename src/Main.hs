{-# LANGUAGE Arrows    #-}
{-# LANGUAGE GADTs     #-}

module Main where

import           Control.Arrow
import           Control.Arrow.Operations
import           Control.Arrow.Transformer.Writer
import           Control.Monad.Identity

-- Type-level naturals
data Zero
data Succ a

data Register = RAX | RBX deriving (Show, Eq)

-- Target machine
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

-- Yay arrows
type X86_64 a b = WriterArrow [Instr] (Kleisli Identity) a b

op :: (Register -> Register -> X86_64 n n) -> X86_64 (Succ (Succ n)) (Succ n)
op instr = pop RAX >>>
           pop RBX >>>
           instr RAX RBX >>>
           push RAX

output :: Instr -> X86_64 n m
output instr = proc _ -> do
              write -< [instr]
              returnA -< undefined

set :: Register -> Int -> X86_64 n n
set r n = output $ Set r n

push :: Register -> X86_64 n (Succ n)
push reg = output $ Push reg

pop :: Register -> X86_64 (Succ n) n
pop reg = output $ Pop reg

add :: Register -> Register -> X86_64 n n
add target param = output $ Add target param

mul :: Register -> Register -> X86_64 n n
mul target param = output $ Mul target param

compile :: Expr t -> X86_64 n (Succ n)
compile (Lit n) = set RAX n >>>
                    push RAX

compile (e :+: e') = compile e >>>
                     compile e' >>>
                     op add

compile (e :*: e') = compile e >>>
                     compile e' >>>
                     op mul

assemble :: X86_64 n (Succ n) -> [Instr]
assemble program = snd $ runIdentity $ runKleisli (elimWriter program) initial
    where initial = undefined :: n


-- Cleanup needed ( maybe fixUntil :: (a -> a -> Bool) -> (a -> a) -> a -> a )
optimize :: [Instr] -> [Instr]
optimize [] = []
optimize (Push r : Pop r' : xs) | r == r' = optimize xs
optimize (Push x : Push y : Pop y' : Pop x': xs) | x == x' && y == y' = optimize xs
optimize (Push x : Push y : Push z : Pop z' : Pop y' : Pop x': xs) | x == x' && y == y' && z == z' = optimize xs
optimize (Set v r : Set v' r' : xs) | r == r' && v == v' = optimize xs
optimize (x : xs) = x : optimize xs

test :: [Instr]
test = optimize $ assemble $ compile (Lit 10 :+: Lit 5 :*: Lit 3)

main :: IO ()
main = print test
