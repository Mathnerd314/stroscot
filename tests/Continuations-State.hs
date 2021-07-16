{-# LANGUAGE  RebindableSyntax, ScopedTypeVariables #-}
import qualified Control.Monad.Trans.Cont as C
import Data.Functor.Identity ( Identity(..) )
import Control.Monad.Fix ( MonadFix(..) )
import Prelude (Monad, Integer, IO, fromInteger, ($), (.), (+), print, const)
import qualified Prelude
import Data.Coerce

type Cont r a = (a -> r) -> r
type Op r = r -> r

unUnit :: Cont r () -> Op r
unUnit f r = f (const r)

e >>= stmts = \c -> e (\x -> stmts x c)
(>>) = (Prelude..)
return x = ($x)

type StateT s m a = Cont (s -> m (a, s)) a

runStateT :: Monad m => Cont (s -> m (a, s)) a -> s -> m (a, s)
runStateT = return (\a s -> Prelude.return (a, s))

--stateT :: Monad m => (s -> m (a, s)) -> Cont (s -> m b) a
stateT f k s = let (>>=) = (Prelude.>>=) in
    f s >>= \(a, s') -> k a s'

state :: Monad m => (s -> (a, s)) -> Cont (s -> m b) a
state f = stateT (Prelude.return . f)

mfixState :: MonadFix m => (a -> Cont (s -> m (a, s)) a) -> Cont (s -> m b) a
mfixState f = stateT $ \ s -> mfix $ \ ~(a, _) -> runStateT (f a) s

get :: Monad m => Cont (s -> m b) s
get = state $ \ s -> (s, s)
put :: Monad m => s -> Op (s -> m b)
put s = unUnit . state $ \_ -> ((), s)

test :: Integer -> Identity (Integer, Integer)
test = runStateT $ do
    s <- get
    put (s+1)
    s <- get
    s <- get
    s <- return s
    put (s+1)
    put (s+1)
    -- () <- put (s+1) -- type error
    -- also put cannot be last statement
    return s

main :: IO ()
main = print (test 0)

pure = return
mapM :: (a -> Cont t2 b) -> [a] -> Cont t2 [b]
mapM f [] = pure []
mapM f (x:xs) = do
    x' <- f x
    xs' <- mapM f xs
    pure (x' : xs')


-- ApplicativeDo
cf <*> cv = \ c ->
    cf $ \ f ->
    cv $ \v ->
     (c $ f v)

fmap f cx = \c ->
    cx (\x ->
    c (f x))

{-

(f <$> ac) <*> bc
= (
    \c ->
    ac (\a ->
    c (f a))
  ) <*> bc
= \c ->
    ac (\a ->
    bc $ \b ->
     (c $ f a b)

vs
ac >>= \a -> bx >>= \b -> return $ f a b
\c -> ac $ \a ->
      bc $ \b ->
        c $ f a b

-}