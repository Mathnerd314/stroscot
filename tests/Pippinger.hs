perm  :: [Int] -> [a] -> [a]
perm ps  = zipWith index ps . repeat

group  :: Int -> [a] -> [[a]]
group n  = unfold (not . null ) (take n) (drop n)

index  :: Int -> [a] -> a
index n  = head . drop n

unfold  :: (a -> Bool ) -> (a -> b) -> (a -> a) -> (a -> [b])
unfold p h t  = map h . takeWhile p . iterate t
 where iterate f x = x: iterate f (f x)

trans :: [[a]] -> [[a]]
trans = foldr (zipWith' (:)) (repeat [])

zipWith' :: (a -> b -> c) -> ([a] -> [b] -> [c])
zipWith' f [] ys = []
zipWith' f (x:xs) ys = f x (head ys) : zipWith'' f xs (tail ys)

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs = zipWith' f xs . unstrictlist
  where unstrictlist = unfold (const True) head tail

doperms :: Int -> [Int] -> [a] -> [a]
doperms n ps = concat . map (perm ps) . group n

doperms' :: Int -> [Int] -> [a] -> [a]
doperms' n ps = concat . map (perm' n ps) . group (2 * n)

perm' :: Int -> [Int] -> [a] -> [a]
perm' n ps = echo (perm ps) . take n
echo :: ([a] -> [a]) -> [a] -> [a]
echo f xs = xs ++ f xs

doperms2' :: Int -> [Int] -> [a] -> [a]
doperms2' n ps = concat . trans . perm' n ps . trans . group (2 * n)


machine :: Eq a => (a, a) -> [a] -> [a]
machine (t, f) xs = head numbers ++ concat (take n (tail numbers)) ++ doperms' n ps (index (n + 1) prologue)
 where
  n= head ns
  ps= take n (tail ns)
  ns = map unary prologue
  numbers = map number prologue
  prologue = iterate after xs
  number xs = takeWhile (== t) xs ++ [stop xs]
  unary = length . takeWhile (== t)
  stop = head . dropWhile (== t)
  after = tail . dropWhile (== t)

testEq :: Eq a => a -> a -> IO ()
testEq x y = if x == y then return () else print "Fail"

main :: IO ()
main = do
  testEq (perm [2, 3, 1, 0] "abcd") "cdba"
  testEq (group 4 "abcdefghijklmnop") ["abcd", "efgh", "ijkl", "mnop"]
  testEq (doperms 4 [2, 3, 1, 0] "abcdefghijklmnop") "cdbaghfekljiopnm"
  testEq (doperms' 4 [2, 3, 1, 0] "abcdefghijklmnop") "abcdcdbaijlkklji"
  testEq (machine ('a', 'b')
    ("aaaab"++"aab"++"aaab"++"ab"++"b" ++ "0123" ++ "xxxx" ++ "4567" ++ "xxxx"))
    ("aaaab"++"aab"++"aaab"++"ab"++"b" ++ "0123" ++ "2310" ++ "4567" ++ "6754")
