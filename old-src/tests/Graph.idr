import Data.Fin

data Graph : Nat -> Type where
  Empty : Graph Z
  Node : Bool -> Graph (S Z)
  Combine : (Fin (S n) -> Fin (S m) -> Bool) -> Graph (S n) -> Graph (S m) -> Graph (S n + S m)

combine : {n:Nat} -> {m:Nat} -> (Fin n -> Fin m -> Bool) -> Graph n -> Graph m -> Graph (n+m)
combine {n=n} _ a Empty = rewrite plusZeroRightNeutral n in a
combine {m=m} _ Empty b = b
combine {n=S n} {m=S m} f a b = Combine f a b

f : Bool -> Fin n -> Fin m -> Bool
f a _ _ = a

overlay : Graph n -> Graph m -> Graph (n+m)
overlay a b = combine (f False) a b

connect : Graph n -> Graph m -> Graph (n+m)
connect a b = combine (f True) a b

unconnected : (n : Nat) -> Graph n
unconnected Z = Empty
unconnected (S n) = overlay (Node False) (unconnected n)

clique : (n : Nat) -> Graph n
clique Z = Empty
clique (S n) = connect (Node True) (clique n)
