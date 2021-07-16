{-# LANGUAGE RebindableSyntax #-}
import Control.Monad.Cont hiding ((>>=),(>>))
import Prelude hiding ((>>=),(>>))

data Task = Print String Task | ReadFile String (String -> Task) | Die

p = Print "Hello World"
a = Print "a"
b = Print "b"
r = ReadFile "f"

type Op = Task -> Task

t2 :: Op
t2 = \c -> a (b c)
(>>) :: Op -> Op -> Op
(>>) e stmts = \c -> e (stmts c)
t2q = a >> b
t3 = \c -> r (\s -> Print s c)

(>>=) :: ((t -> Task) -> Task) -> (t -> Op) -> Op
(>>=) e pstmts = \c -> e (\x -> pstmts x c)

t3q :: Op
t3q = r >>= Print

stuff :: Op
stuff = do
  a
  b
  s <- r
  a

