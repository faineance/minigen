module Main where
import           Control.Monad.Writer
import           Control.Monad.Identity
import Expr
import CodeGen
import Optimization

test :: [Instr]
test =  optimize $ assemble $ compile $ optimize (Lit 10 :+: Lit 5 :*: Lit 3)

main :: IO ()
main = print test
