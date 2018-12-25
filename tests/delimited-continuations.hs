-- Some helpers
abortP p e = withSubCont p (\_ -> e)
promptP f = p = newPrompt; pushPrompt p (f p)
controlP p f = withSubCont p
                 \sk -> pushPrompt p (f (\c -> pushSubCont sk c))
shiftP p f = withSubCont p
               \sk -> pushPrompt p (f (\c -> pushPrompt p (pushSubCont sk c)))

---

tailtest = 
    do p = newPrompt
       loop p
    where loop p = pushPrompt p $ 
                   withSubCont p $ \s -> 
                   pushSubCont s (loop p)
-- infinite loop, but shouldn't blow the stack

--------------------------------------------------------------------------
-- Tests...

test1 = runCC (return 1 >>= (return . (+ 4)))
-- 5

test2 = runCC (do
	       p <- newPrompt
	       (pushPrompt p (pushPrompt p (return 5)))
	       >>= (return . (+ 4)))
-- 9

test3 = runCC (do
	       p <- newPrompt
	       (pushPrompt p (abortP p (return 5) >>= (return . (+ 6))))
	       >>= (return . (+ 4)))
-- 9

test4 = runCC (do
	       p <- newPrompt
	       (pushPrompt p 
		(pushPrompt p (abortP p (return 5) >>= (return . (+ 6))))
		>>= (return . (+ 4))))
-- 9

test5 = runCC (do
	       p <- newPrompt
	       v <- pushPrompt p $ 
		 do
		 v1 <- pushPrompt p (abortP p (return 5) >>= (return . (+ 6)))
		 v1 <- abortP p (return 7)
		 return $ v1 + 10
	       return $ v + 20)
-- 27

-- From Zena Ariola:
test6 = 
  runCC (
    do p <- newPrompt
       pushPrompt p $ 
         do fun <- return (\x -> shiftP p (\f1 -> do a <- f1 x
                                                     return (4 - a)))
            arg <- do b <- shiftP p (\f2 -> do a <- f2 (return 7)
                                               return (2 + a))
                      return (return (3 + b))
            a <- fun arg 
            return (5 + a))

-- -9
-- with F => -13

test7 = runCC (do
	       p <- newPrompt
	       v <- pushPrompt p $ 
		 do
	         v1 <- withSubCont p $ \sk -> pushPrompt p (pushSubCont sk (return 5))
	         return $ v1 + 10
	       return $ v + 20)
-- 35

--(display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
--; --> 117

test8 = runCC (
	       do
	       p0 <- newPrompt
	       p1 <- newPrompt
	       pushPrompt p0 (do
				 v <- shiftP p0 $
				      \sk -> 
				         do v1 <- sk (sk (return 3))
				            return (100 + v1)
				 return (v + 2))
	       >>= (return . (10 +)))

-- 117

test9 = runCC (
	       do
	       p0 <- newPrompt
	       p1 <- newPrompt
	       pushPrompt p0 (do
				 v <- shiftP p0 $
				      \sk -> 
				         do v1 <- sk (return 3)
				            return (100 + v1)
				 return (v + 2))
	       >>= (return . (10 +)))
-- 115

test10 = runCC (
	       do
	       p0 <- newPrompt
	       p1 <- newPrompt
	       pushPrompt p0 (do
				 v <- shiftP p0 $
				      \sk -> 
				         do v1 <- sk (pushPrompt p1
						      (sk (abortP p1 
							   (return 3))))
				            return (100 + v1)
				 return (v + 2))
	       >>= (return . (10 +)))
-- 115

test11 = runCC (
	   do
	   p1 <- newPrompt
	   p2 <- newPrompt
	   let pushtwice sk = do
	                      pushSubCont sk (pushSubCont sk (return 3))
	   v <- pushPrompt p1 (
			       do
			       v2 <- pushPrompt p2 (withSubCont p1 pushtwice)
			       return $ v2 + 1)
	   return $ v + 10)
-- 15

test12 = runCC (
	   do
	   p1 <- newPrompt
	   p2 <- newPrompt
	   p3 <- newPrompt
	   let pushtwice sk = do
	                      pushSubCont sk (pushSubCont sk 
					      (withSubCont p2
					       (\sk2 -> pushSubCont sk2
						(pushSubCont sk2 (return 3)))))
	   v <- pushPrompt p1 (
			       do
			       v2 <- pushPrompt p2 
			              (do
				       v3 <- pushPrompt p3
						    (withSubCont p1 pushtwice)
				       return $ v3 + 10)
			       return $ v2 + 1)
	   return $ v + 100)
-- 135

test13 = runCC (
	   do
	   p1 <- newPrompt
	   p2 <- newPrompt
	   p3 <- newPrompt
	   let pushtwice f = f (f (shiftP p2 (\f2 -> f2	(f2 (return 3)))))
	   v <- pushPrompt p1 (
			       do
			       v2 <- pushPrompt p2 
			              (do
				       v3 <- pushPrompt p3
						    (shiftP p1 pushtwice)
				       return $ v3 + 10)
			       return $ v2 + 1)
	   return $ v + 100)
-- 135

-- (reset (let ((x (shift f (cons 'a (f '()))))) (shift g x))))
-- ==> '(a)

test14 = 
  runCC (
    do
    p <- newPrompt
    pushPrompt p (
		  do
		  let x = shiftP p (\f -> f (return []) >>= (return . ("a":)))
		  xv <- x
		  shiftP p (\_ -> return xv)))

-- ["a"]
		
test15 = 
  runCC (
    do
    p <- newPrompt
    pushPrompt p (
		  do
		  let x = controlP p (\f -> f (return []) >>= (return . ("a":)))
		  xv <- x
		  controlP p (\_ -> return xv)))


-- []

test16 = 
  runCC (
    do
    p <- newPrompt
    pushPrompt p (
		  do
		  let x = controlP p (\f -> f (return []) >>= (return . ("a":)))
		  xv <- x
		  controlP p (\g -> g (return xv))))
-- ["a"]

test17 = 
  runCC (
    do
    p <- newPrompt
    pushPrompt p (do
		  withSubCont p (\sk -> 
				pushPrompt p (pushSubCont sk (return 1)))
		  withSubCont p (\sk -> pushSubCont sk (return 2))))
-- 2

-- Traversing puzzle by Olivier Danvy

type DelimControl a b = 
    forall r. Prompt r b -> ((CC r a -> CC r b) -> CC r b) -> CC r a

traverse :: DelimControl [a] [a] -> [a] -> [a]
traverse op lst =
    runCC (do
	   p <- newPrompt
	   let visit [] = return []
	       visit (h:t) = do
	                      v <- op p (\f -> f (return t) >>= (return . (h:)))
	                      visit v
	   pushPrompt p (visit lst))

test18 = traverse shiftP [1,2,3,4,5]
-- [1,2,3,4,5]

test19 = traverse controlP [1,2,3,4,5]
-- [5,4,3,2,1]

-- All the following return
-- *** Exception: Prompt was not found on the stack

test20 = runCC (do
	       p <- newPrompt
	       v <- pushPrompt p $ 
		 do
		 v1 <- pushPrompt p (abortP p (return 5) >>= (return . (+ 6)))
		 v1 <- abortP p (return 7)
		 return $ v1 + 10
	       v <- abortP p (return 9)
	       return $ v + 20)

test21 = runCC (do 
                p <- newPrompt
                abortP p (return 5))

test22 = runCC (do
                p <- newPrompt
                pushPrompt p $ 
                  do 
                    x <- withSubCont p (\_ -> abortP p (return 5))
                    return 0)
