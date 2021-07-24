{-# LANGUAGE TupleSections, NoMonomorphismRestriction #-}

import Control.Applicative
import Data.Char(isSpace)
import Data.Function(on)
import Data.List
import Data.List.Split(splitOn)
import Data.Ord(comparing)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe(catMaybes)
import Control.Arrow(first)
import Control.Monad(join)
import System.Environment(getArgs)
import Prelude hiding (max)

type Object = String
data Group = Group String (Set Object) deriving (Eq,Ord)

instance Show Group where
  show (Group s l) = s

-- Remove groups
flatten :: Group -> Set Object
flatten (Group _ xs) = xs

subsetOf :: Group -> Group -> Bool
subsetOf x y = flatten x `S.isSubsetOf` flatten y

data PrecedesX a = P a a deriving (Eq,Show,Ord)

type PrecG = PrecedesX Group
type Precedes = PrecedesX Object

data Power = Power (Set PrecG) -- power is the least upper bound (maximum / supremum) of the set
   deriving Eq

-- Inverse of specificity
power :: PrecG -> Power
power = Power . S.singleton

max :: Power -> Power -> Power
max (Power p1) (Power p2) = Power (S.fromList . takeMaxes . S.toList $ S.union p1 p2)

takeMaxes :: [PrecG] -> [PrecG]
takeMaxes l = takeMaxes' l []
  where
   -- 'xs' is the list of things we've seen so far,
   -- 'y' is the potential new element
   -- 'ys' is the rest of the list
   takeMaxes' [] _         = []
   takeMaxes' (y:ys) xs
      |  y `dominatedBy` (ys++xs) = takeMaxes' ys xs
      | otherwise       = y : takeMaxes' ys (y:xs)

   dominatedBy _ []         =  False
   dominatedBy y (x:xs)     =  y `subsetOfPrec` x || y `dominatedBy` xs

subsetOfPrec :: PrecG -> PrecG -> Bool
subsetOfPrec (P a b) (P c d) =
   (a `subsetOf` c && b `subsetOf` d && (a /= c || b /= d))
   || (a `subsetOf` d && b `subsetOf` c && (a /= d || b /= c))

powerUp :: [PrecG] -> [(PrecG, Power)]
powerUp list = zip list (map power list)

-- a X b and b X c -> a X c
transitivity :: (Precedes,Power) -> (Precedes,Power) -> [(Precedes,Power)]
transitivity (P w x, p) (P y z, p2) = f ++ g where
    f = if (x == y) then [(P w z, max p p2)] else []
    g = if (w == z) then [(P y x, max p p2)] else []

-- test all possible pairs
step0 :: [(Precedes, Power)] -> [(Precedes, Power)]
step0 = concat . (uncurry $ liftA2 transitivity) . join (,)

data Order = O Bool Object Object  deriving (Show)

-- finds highest power of each relation
nub2 :: [(Precedes, Power)] -> [(Precedes, Power)]
nub2 = nubBy ((==) `on` fst) . sortBy (comparing snd)

-- fill in pairs, appending to end of list
p :: [(Precedes, Power)] -> [(Precedes, Power)]
p = nub2 . ((++) <*> step0)

-- until lowest power for each has been found
fix :: Eq a => (a -> a) -> a -> a
fix = until =<< ((==) <*>)
q :: [(Precedes, Power)] -> [(Precedes, Power)]
q = fix p

-- P (Group [a,b,...] [c,d,...]) -> [P a c, P a d, ..., P b c, P b d, ...]
split :: [(PrecG, Power)] -> [(Precedes, Power)]
split  = (=<<) (\(P x y,p) -> map (,p) $ liftA2 P (S.toList $ flatten x) (S.toList $ flatten y))

-- cancel out pairs P x y  P y x, also flipping them for topological sort
double :: [(Precedes, Power)] -> [(Order, Power)]
double = (=<<) (\(P x y,p) -> [(O True x y,p),(O False y x,p)])
reduce ::[(Order, Power)] -> [Precedes]
reduce = (=<<) (\((k1,k2),(b,v)) -> if b then [] else [P k1 k2]) . M.toList . foldr (\(O b k1 k2, v) -> M.alter (f b v) (k1,k2)) M.empty where
   f b1 v1 (Just (b2,v2)) = case (compare v1 v2) of
                              LT -> Just (b1,v1)
                              GT -> Just (b2,v2)
                              EQ -> if (b1 == b2) then Just (b1,v1) else Nothing
   f b1 v1 Nothing = Just (b1,v1)

collect :: [Precedes] -> [([Object], [Object])]
collect = M.toList . foldr (\(P k v) -> M.insertWith (++) [k] [v]) M.empty

solve :: [PrecG] -> [Object]
solve = toposort . collect . reduce . double . nub2 . q . split . powerUp

-- From http://rosettacode.org/wiki/Topological_sort#Haskell (modified to be slightly more generific)
combs :: (Eq t, Num t) => t -> [a] -> [[a]]
combs 0 _ = [[]]
combs _ [] = []
combs k (x:xs) = map (x:) (combs (k-1) xs) ++ combs k xs

toposort :: (Show a, Eq a) => [([a], [a])] -> [a]
toposort xs
      | (not.null) cycleDetect = error $ "Dependency cycle detected" ++ show cycleDetect
      | otherwise              = foldl makePrecede [] dB

   where dB = map (\(x,y) -> (x,y \\ x)) xs

         makePrecede ts ([x],xs)  = nub $ case elemIndex x ts of
                                          Just i  -> uncurry(++) $ first(++xs) $ splitAt i ts
                                          _       -> ts ++ xs ++ [x]

         cycleDetect = filter ((>1).length)
                       $ map (\[(a,as), (b,bs)] -> (a `intersect` bs) ++ (b `intersect`as))
                       $ combs 2 dB

---------------------------------------------------------------------------
-- I/O

-- Group/Object
parseObject dict (name,value) = if (last name) == ':'
                                  then M.insert
                                          (reverse . tail . reverse $ name)
                                          (Group name (S.unions (map flatten (catMaybes (liftA2 M.lookup (words value) [dict]))))) dict
                                  else M.insert name (Group name (S.singleton value)) dict

-- Constraints
parseConstraints dict = map (\(name,value) -> P (dict M.! name) (dict M.! value))

process input = parseConstraints dict constraints where
  [objs, constrnts] = splitOn "--constraints--\n" input
  stuff = [(trim a,trim b) | [a,b] <- map (splitOn "=") (lines objs) ]
  dict  = foldl' parseObject M.empty stuff
  constraints = [(trim a,trim b) | [a,b] <- map (splitOn ":") (lines constrnts)]
  trim  = f . f
  f = reverse . dropWhile isSpace

main :: IO ()
main = do
   i <- input
   let answer = solve (process i)
   mapM_ print answer

input = do
    xs <- getArgs
    if length xs > 0 then readFile (xs !! 0) else getContents

-- debugging: output $ fmap (solve.process) (readFile "...")
output = join . fmap (mapM_ print)