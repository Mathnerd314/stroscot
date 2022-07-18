type Name = String
data Term = Var Name | Con Int | Add Term Term | Lam Name Term | App Term Term
data Value = Wrong | Num Int | Fun (M Value -> M Value)
type Environment = [(Name, M Value)]

type M a = S a

type S a = State -> (a, State)
data State = State Int

type K a = (a -> Answer) -> Answer

type Answer = M Value

type CPS a = (a -> S Value) -> S Value

test :: CPS a
test = \_ -> \s -> (Wrong, s)
-- wadlerEssenceFunctionalProgramming1992 section 3.4:
-- This provides an error escape: it ignores the current continuation and always returns Wrong.