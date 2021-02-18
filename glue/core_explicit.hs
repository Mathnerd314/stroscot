type Tag = String
type EID = String
type Gamma = [EID]
type Delta = [EID]
type Theta = [EID]
type Lambda = [EID]
type A = EID
type B = EID
type T = EID

type Level = Integer
type I = Level
type J = Level

type Var = String

data Sequent left right = Sequent
  { turnstile :: EID
  , left :: left
  , right :: right
  }

data Rule top bottom = Rule
  { bottom :: bottom
  , top :: top
  }

-- this version of the syntax has slots for all of the contexts
data Syntax
  = PiR (Rule
      [(Tag, Sequent (Gamma, [A]) ([B], Delta))]
      (Sequent Gamma (T, Delta))
      )
  | PiL Tag (Rule
      ([Sequent Gamma (A,Delta)], [Sequent (Theta, B) Lambda])
      (Sequent ([Gamma], [Theta], T) ([Delta], [Lambda]))
      )
  | SigmaR Tag (Rule
      ([Sequent (Gamma, B) Delta], [Sequent Theta (A, Lambda)])
      (Sequent ([Gamma], [Theta]) (T, [Delta], [Lambda]))
      )
  | SigmaL (Rule
      [(Tag, Sequent (Gamma, [A]) ([B], Delta))]
      (Sequent (Gamma, T) Delta)
      )
  | Bang I J (Rule
      (Sequent Gamma (A, Delta))
      (Sequent Gamma (T, Delta))
      )
  | BangD I J (Rule
      (Sequent (Gamma, A) Delta)
      (Sequent (Gamma, T) Delta)
      )
  | BangC (Rule
      (Sequent (Gamma, [T]) Delta)
      (Sequent (Gamma, T) Delta)
      )
  | BangW (Rule
      (Sequent Gamma Delta)
      (Sequent (Gamma, T) Delta)
      )
  | Whim I J (Rule
      (Sequent (Gamma, A) Delta)
      (Sequent (Gamma, T) Delta)
      )
  | WhimD I J (Rule
      (Sequent Gamma (A, Delta))
      (Sequent Gamma (T, Delta))
      )
  | WhimC (Rule
      (Sequent Gamma ([T], Delta))
      (Sequent Gamma (T, Delta))
      )
  | WhimW (Rule
      (Sequent Gamma Delta)
      (Sequent Gamma (T, Delta))
      )
  | Identity (Rule () (Sequent A A))
  | Cut (Rule
      (Sequent Gamma (A, Delta), Sequent (Theta, A) Lambda)
      (Sequent (Gamma, Theta) (Delta, Lambda))
      )
  | Use Var (Rule () (Sequent Gamma Delta))
  | Assign Var (Rule (Sequent Gamma Delta) ())
