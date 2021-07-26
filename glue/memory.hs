{-#LANGUAGE LambdaCase, ScopedTypeVariables, TupleSections #-}
import Prelude hiding (Read)
import Data.Bits hiding (Bits, shift)
import Data.List
import Data.Function(on)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad hiding (join)
import System.Random

-- data Word = Word { value :: Integer } -- value < some fixed max

data MaskedWord = MaskedWord { word :: Word, mask :: Word }
  deriving (Show,Eq,Ord)
masked0 (MaskedWord w m) = w .|. m
masked1 (MaskedWord w m) = w .&. (complement m)

fullMask = complement 0

type Addr = Word
data Fresh a
  = FreshAddr (Addr -> Fresh a)
  | Write { unW :: a }

instance Show a => Show (Fresh a) where
  show (FreshAddr _) = "<Fresh>"
  show (Write a) = show a

type Write = Fresh (Map Addr MaskedWord)

instance Functor Fresh where
  fmap = liftM

instance Applicative Fresh where
  pure = Write
  (<*>) = ap

instance Monad Fresh where
  Write a >>= f = f a
  FreshAddr fa >>= f = FreshAddr (\a -> fa a >>= f)

runFresh :: Fresh a -> IO ([Addr],a)
runFresh (Write a) = pure ([],a)
runFresh (FreshAddr fa) = do
  addr <- randomIO
  (ls, r) <- runFresh $ fa addr
  pure $ (addr:ls,r)

writeBounds :: Write -> Fresh (Maybe (Addr, Addr))
writeBounds wr = do
  w <- wr
  let ws = M.keys w
  pure $ mkInfo ws
    where
      mkInfo [] = Nothing
      mkInfo xs = Just (start, end+1)
        where
          (start,end) = foldl1 mergeAddr (map (\i -> (i,i)) xs)
          mergeAddr (min1,max1) (min2,max2) = (min1 `min` min2, max1 `max` max2)

join :: Write -> Write -> Write
join pa pb = do
  writeBounds pa >>= \case
    Nothing -> pb
    Just (_,aend) -> writeBounds pb >>= \case
      Nothing -> pa
      Just (bstart,_) -> do
        let chg = (+(aend-bstart))
        a <- pa
        b <- pb
        Write $ a `M.union` M.mapKeys chg b

j :: Write -> Write -> Write
j (FreshAddr a) (FreshAddr b) = FreshAddr $ \addr -> join (a addr) (b addr)
j a b = join a b

moveBase :: Addr -> Write -> Write
moveBase addr (FreshAddr fa) =  fa addr
moveBase _ w = w

togetherWith :: Write -> Write -> Write
togetherWith pa pb = do
  a <- pa
  b <- pb
  Write $ a `M.union` b

class Pack a where
  pack :: a -> (Write, Unpack a)
  -- convention: the base address is the first fresh addr

type Read = Map Addr Word

toRead :: Map Addr MaskedWord -> Map Addr Word
toRead = M.map word

type Unpack a = Addr -> Read -> a

shift :: Addr -> Addr -> Read -> Read
shift adr start r = M.mapKeys (+(adr-start)) r

end :: Pack a => Addr -> a -> Addr
end adr a = do
  let FreshAddr frb = writeBounds (pack a)
  let Write (Just (_,end)) = frb adr
  end

-- Some simple memory layouts:
instance Pack Word where
  pack w = (,unpack) . FreshAddr $ \base ->
    Write (M.singleton base (MaskedWord w fullMask))
    where
      unpack a r = fromJust $ M.lookup a r

instance (Pack a, Pack b) => Pack (a,b) where
  pack (a,b) = do
    let (pa, upa) = pack a
    let (pb, upb) = pack b
    (j pa pb, unpack)
      where
        unpack adr r = do
          let a = upa adr r :: a
          let b = upb adr (shift adr (end adr a) r)
          (a,b)

instance (Pack a, Pack b) => Pack (Either a b) where
  pack (Left a) = (,unpackE) $ j (pack (0 :: Word)) (pack a)
  pack (Right b) = (,unpackE) $ j (pack (1 :: Word)) (pack b)

unpackE adr r = do
    let s = unpack adr r :: Word
    case s of
      0 -> Left (unpack adr (shift adr (end adr s) r))
      1 -> Right (unpack adr (shift adr (end adr s) r))
      _ -> undefined

-- We can encode a list in a number of ways:
-- examples are for pack ["a","b"]

newtype FlatL a = FlatL [a]
  deriving (Show,Eq,Ord)
-- flat list, stored like [2,"a","b"]

instance Pack a => Pack (FlatL a) where
  pack (FlatL ls) = (,unpack) $ do
    let l = pack (fromIntegral (length ls) :: Word)
    foldl j l (map pack ls)
    where
      unpack adr r = do
        let l = unpack adr r :: Word
        let unp 0 _ = []
            unp n r = do
              let x = unpack adr r :: a
              x : unp (n-1) (shift adr (end adr x) r)
        FlatL $ unp l (shift adr (end adr l) r)

newtype Flat2 a = Flat2 [a]
  deriving (Show,Eq,Ord)
-- flat list, stored like [1,"a",1,"b",0]

instance Pack a => Pack (Flat2 a) where
  pack (Flat2 []) = pack (0 :: Word)
  pack (Flat2 (x:xs)) = pack (1 :: Word) `j` pack x `j` pack (Flat2 xs)

instance (Unpack a, Pack a) => Unpack (Flat2 a) where
  unpack adr r = do
    let unp adr r = do
          let x = unpack adr r :: Word
          case x of
            0 -> []
            1 -> do
              let r' = (shift adr (end adr x) r)
              let x = unpack adr r' :: a
              x : unp adr (shift adr (end adr x) r')
            _ -> undefined
    Flat2 $ unp adr r

newtype IntL a = IntL [a]
  deriving (Show,Eq,Ord)
-- intrusive list, stored like x=[1,"a",&y], y=[1,"b",&z], z=[0]

instance Pack a => Pack (IntL a) where
  pack (IntL []) = pack (0 :: Word)
  pack (IntL (x:xs)) =
    FreshAddr $ \base ->
    FreshAddr $ \addr -> do
    let sub = pack (IntL xs)
    moveBase base (pack (1 :: Word) `j` pack x `j` pack addr)
     `togetherWith` moveBase addr sub

instance (Unpack a, Pack a) => Unpack (IntL a) where
  unpack adr r = do
    let unp adr r = do
          let x = unpack adr r :: Word
          case x of
            0 -> []
            1 -> do
              let r' = shift adr (end adr x) r
              let v = unpack adr r' :: a
              let r'' = shift adr (end adr v) r'
              let w = unpack adr r'' :: Word
              v : unp w r
            _ -> undefined
    IntL $ unp adr r

newtype UnifL a = UnifL [a]
  deriving (Show,Eq,Ord)
-- uniform list, stored like x=[1,&x1,&y],x1="a",y = [1,&y1,&z],y1="b", z=[0]

instance Pack a => Pack (UnifL a) where
  pack (UnifL []) = pack (0 :: Word)
  pack (UnifL (x:xs)) =
    FreshAddr $ \base ->
    FreshAddr $ \next ->
    FreshAddr $ \elem -> do
    let part = pack (1 :: Word)
    let e = pack x
    let sub = pack (UnifL xs)
    moveBase base (part `j` pack elem `j` pack next) `togetherWith` moveBase elem e `togetherWith` moveBase next sub

instance (Unpack a, Pack a) => Unpack (UnifL a) where
  unpack adr r = do
    let unp adr r = do
          let x = unpack adr r :: Word
          case x of
            0 -> []
            1 -> do
              let r' = shift adr (end adr x) r
              let v = unpack adr r' :: Word
              let r'' = shift adr (end adr v) r'
              let w = unpack adr r'' :: Word
              unpack v r : unp w r
            _ -> undefined
    UnifL $ unp adr r


repack :: (Pack a, Unpack a, Eq a, Show a) => a -> IO Read
repack a = do
  (as, x) <- runFresh (pack a)
  let a' = unpack (head as) (toRead x)
  case a == a' of
    True -> pure (toRead x)
    False -> do
      putStrLn $ "Failed: " ++ show a
      pure (toRead x)

tuple = (1,2) :: (Word,Word)
eith = Left 1 :: Either Word Word
list = [1,2] :: [Word]
flatl = FlatL list
flat2 = Flat2 list
intl = IntL list
unifl = UnifL list

test = do
  repack tuple
  repack eith
  repack flatl
  repack flat2
  repack intl
  repack unifl
  pure ()
