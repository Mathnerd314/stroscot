
import Data.Function
import Data.List.Extra
import Data.Maybe(fromJust)
import Unsafe.Coerce
import Control.Concurrent.Extra
import Control.Exception.Extra
import Data.Tuple.Extra
import Data.IORef
import Control.Monad.Extra
import Control.Monad.IO.Class
import System.Clock
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Heap as Heap
import Data.Map(Map)
import Data.Set(Set)
import Data.Heap(Heap)
import GHC.Exts(Any)
import Control.Monad.State
import Control.Monad.IO.Unlift
import GHC.Stack

-- | An 'IO' action that when evaluated calls 'assert' in the 'IO' monad, which throws an 'AssertionFailed' exception if the argument is 'False'.
--
-- > catch (assertIO True  >> pure 1) (\(x :: AssertionFailed) -> pure 2) == pure 1
-- > catch (assertIO False >> pure 1) (\(x :: AssertionFailed) -> pure 2) == pure 2
-- > seq (assertIO False) (print 1) == print 1
assertIO :: Partial => Bool -> IO ()
assertIO x = withFrozenCallStack $ evaluate $ assert x ()

main = do
  x <- getLine -- a complex computation
  assertIO (length x > 3)
  print "done"