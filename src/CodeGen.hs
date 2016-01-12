{-# LANGUAGE Arrows    #-}
{-# LANGUAGE GADTs     #-}
module CodeGen where
import           Control.Arrow
import           Control.Arrow.Operations
import           Control.Arrow.Transformer.Writer
import           Control.Monad.Identity
import Expr

type X86_64 a b = WriterArrow [Instr] (Kleisli Identity) a b

op :: (Register -> Register -> X86_64 n n) -> X86_64 (Succ (Succ n)) (Succ n)
op instr = pop EAX >>>
           pop EBX >>>
           instr EAX EBX >>>
           push EAX

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
add r r' = output $ Add r r'

mul :: Register -> Register -> X86_64 n n
mul r r' = output $ Mul r r'

compile :: Expr t -> X86_64 n (Succ n)
compile (Lit n) = set EAX n >>>
                    push EAX

compile (e :+: e') = compile e >>>
                     compile e' >>>
                     op add

compile (e :*: e') = compile e >>>
                     compile e' >>>
                     op mul

assemble :: X86_64 n (Succ n) -> [Instr]
assemble program = snd $ runIdentity $ runKleisli (elimWriter program) initial
    where initial = undefined :: n
