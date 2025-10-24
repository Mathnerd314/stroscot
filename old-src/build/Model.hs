import Data.ByteString ( ByteString )
import Data.Map(Map)

type ThunkName = ByteString
type KeyValue = ByteString
type KeyName = ByteString
data ThunkOperation = Call [ThunkName]

type Task = Store -> Trace KeyValue KeyValue
type Tasks = ThunkName -> Task

data Trace r w = Trace
    { reads :: [(KeyName, r)]
    , writes :: [(KeyName, w)]
    , operation  :: ThunkOperation }

data Store = Store { info :: Map ThunkName (Trace Hash Hash), values :: Map KeyName KeyValue }

type Build = Tasks -> ThunkName -> Store -> Store

newtype Hash = Hash KeyValue
