-- Some helpers
abortP p e = withSubCont p (\_ -> e)
promptP f = { p = newPrompt; pushPrompt p (f p) }
controlP p f = withSubCont p
                 \sk -> pushPrompt p (f (\c -> sk c))
shiftP p f = withSubCont p
               \sk -> pushPrompt p (f (\c -> pushPrompt p (sk c)))

---

tailtest:
    p = newPrompt
    loop p
      where loop p = pushPrompt p $ withSubCont p $ \s -> s (loop p)
-- infinite loop, but shouldn't blow the stack

--------------------------------------------------------------------------
-- Tests...

test1: 4+1
-- 5

test2 = { p <- newPrompt; 4+(pushPrompt p (pushPrompt p 5)) }
-- 9

test3 = { p <- newPrompt; 4+(pushPrompt p (6 + abortP p 5)) }
-- 9

test4 = { p <- newPrompt; 4+(pushPrompt p (pushPrompt p (6 + abortP p 5))) }
-- 9

test5:
    p <- newPrompt
    v <- pushPrompt p $
        x <- pushPrompt p (6 + abortP p 5)
        t <- abortP p 7
        undefined
    v + 20
-- 27=7+20

-- From Zena Ariola:
test6:
    p <- newPrompt
    pushPrompt p $ 
        fun <- \x -> shiftP p (\f1 -> 4 - f1 x)
        arg <- 3 + shiftP p (\f2 -> 2 + f2 7)
        5 + fun arg

-- -9 = 2+4-(5+3+7)

test7:
    p <- newPrompt
    pushPrompt p (withSubCont p (\sk -> pushPrompt p (sk 5)) + 10) + 20
-- 35

--(display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
--; --> 117

test8:
    p0 <- newPrompt
    p1 <- newPrompt
    10 + pushPrompt p0 (2 + (shiftP p0 (\sk -> 100 + sk (sk 3))))
-- 117

test9:
    p0 <- newPrompt
    p1 <- newPrompt
    10 + pushPrompt p0 (2 + (shiftP p0 (\sk -> 100 + sk 3)))
-- 115

test10:
    p0 <- newPrompt
    p1 <- newPrompt
    10 + pushPrompt p0 (2 + (shiftP p0 (\sk -> 100 + sk (pushPrompt p1 (sk (abortP p1 3))))))
-- 115

test11:
    p1 <- newPrompt
    p2 <- newPrompt
    let pushtwice sk = sk (sk 3)
    pushPrompt p1 (pushPrompt p2 (withSubCont p1 pushtwice) + 1) + 10
-- 15 = 3+1+1+10

test12:
    p1 <- newPrompt
    p2 <- newPrompt
    p3 <- newPrompt
    let pushtwice sk = sk (sk (withSubCont p2 (\sk2 -> sk2 (sk2 3))))
    pushPrompt p1 (pushPrompt p2 (pushPrompt p3 (withSubCont p1 pushtwice) + 10) + 1) + 100)
-- 135

test13:
    p1 <- newPrompt
    p2 <- newPrompt
    p3 <- newPrompt
    let pushtwice f = f (f (shiftP p2 (\f2 -> f2 (f2 3))))
    pushPrompt p1 (pushPrompt p2 (pushPrompt p3 (shiftP p1 pushtwice) + 10) + 1) + 100)
-- 135

-- (reset (let ((x (shift f (cons 'a (f '()))))) (shift g x))))
-- ==> '(a)

test14:
    p <- newPrompt
    pushPrompt p (let x = shiftP p (\f -> "a" : f [])
                  shiftP p (\_ -> x)))

-- ["a"]
                
test15:
    p <- newPrompt
    pushPrompt p (let x = controlP p (\f -> "a" : f [])
                  controlP p (\_ -> x))


-- []

test16:
    p <- newPrompt
    pushPrompt p (let x = controlP p (\f -> "a" : f [])
                  controlP p (\g -> g x))
-- ["a"]

test17:
    p <- newPrompt
    pushPrompt p (_ <- withSubCont p (\sk -> pushPrompt p (sk 1))
                  withSubCont p (\sk -> sk 2))
-- 2

-- Traversing puzzle by Olivier Danvy

type DelimControl a b = forall r. Prompt r b -> ((CC r a -> CC r b) -> CC r b) -> CC r a

traverse :: DelimControl [a] [a] -> [a] -> [a]
traverse op lst:
    p <- newPrompt
    let visit [] = []
        visit (h:t) = visit (op p (\f -> h : f t))
    pushPrompt p (visit lst)

test18 = traverse shiftP [1,2,3,4,5]
-- [1,2,3,4,5]

test19 = traverse controlP [1,2,3,4,5]
-- [5,4,3,2,1]

-- All the following return
-- *** Exception: Prompt was not found

test20:
    p <- newPrompt
    _ <- pushPrompt p $ 
        _ <- pushPrompt p (abortP p 5 + undefined)
        _ <- abortP p 7
        undefined
    _ <- abortP p 9 -- causes error
    v + 20

test21:
    p <- newPrompt
    abortP p 5 -- causes error

test22:
    p <- newPrompt
    pushPrompt p $ withSubCont p (\_ -> abortP p 5)
