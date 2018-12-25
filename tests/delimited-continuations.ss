;;; Definitions of some common delimited control operators

(define P (newPrompt))

(define-syntax prompt
  (syntax-rules ()
    [(_ e1 e2 ...)
     (pushPrompt P e1 e2 ...)]))

(define F ; aka +F-
 ; captures up to but not including prompt
 ; aborts up to but not including prompt
 ; i.e., leaves prompt in uncaptured/unaborted portion of the continuation
  (lambda (proc)
    (withSubCont P
      (lambda (s)
        (pushPrompt P
          (proc (lambda (v) (pushSubCont s v))))))))

(define-syntax control
  (syntax-rules ()
    ((_ x e1 e2 ...) (F (lambda (x) e1 e2 ...)))))

;;; variations on F
;;; -F-: aborts past prompt; does not reinstate prompt on throw
;;; -F+: aborts past prompt; does reinstate prompt on throw
;;; +F-: (original F) leaves prompt; does not reinstate prompt on throw
;;; +F+: (shift) leaves prompt; reinstates on throw

(define subcontinuation
  (lambda (p restore? s)
     (lambda (v)
       (if restore?
           (pushPrompt p (pushSubCont s v))
           (pushSubCont s v)))))

(define controller
  (lambda (p leave? restore?)
    (lambda (proc)
      (withSubCont p
        (lambda (s)
          (if leave?
              (pushPrompt p (proc (subcontinuation p restore? s)))
              (proc (subcontinuation p restore? s))))))))

(define -F- (controller P #f #f))
(define -F+ (controller P #f #t))
(define +F- (controller P #t #f))
;(define F +F-)
(define +F+ (controller P #t #t))

; alternatively, once we have -F-
;(define -F+
;  (lambda (p)
;    (-F- (lambda (k)
;           (p (lambda (v)
;                (prompt (k v))))))))
;(define +F-
;  (lambda (p)
;    (-F- (lambda (k)
;           (prompt (p k))))))
;(define +F+
;  (lambda (p)
;    (-F- (lambda (k)
;           (prompt
;             (p (lambda (v)
;                  (prompt (k v)))))))))

;;; shift and reset
(define-syntax reset
  (syntax-rules ()
    ((_ e1 e2 ...) (prompt e1 e2 ...))))

(define-syntax shift
  (syntax-rules ()
    ((_ k e1 e2 ...) (+F+ (lambda (k) e1 e2 ...)))))

;;; spawn
(define spawn
  (lambda (proc)
    (let ((p (newPrompt)))
      (pushPrompt p
        (proc (controller p #f #t))))))

;;; From Section 2.1

((lambda (p)
   (+ 2
      (pushPrompt p
        (if (withSubCont p
              (lambda (k)
                (+ (pushSubCont k #f)
                   (pushSubCont k #t))))
            3
            4))))
 (newPrompt)) ;=> 9

;;; From Section 5.2

(define (tailtest)
  (let ([p (newPrompt)])
    (pushPrompt p
      (withSubCont p
        (lambda (s)
          (pushSubCont s (tailtest)))))))

(tailtest) ;=> <nonterminating>

;; additional tail tests

(define (tailtest1a)
  ; same as above, but push same prompt each time
  (let ([p (newPrompt)])
    (let loop ()
      (pushPrompt p
        (withSubCont p
          (lambda (s)
            (pushSubCont s (loop))))))))

(tailtest1a) ;=> <nonterminating>

(define (tailtest2)
  ; repeatedly capture but don't push an nonempty subcontination
  (let ([p (newPrompt)])
    (let loop ()
      (pushPrompt p
        ((withSubCont p
           (lambda (s)
             (loop))))))))

(tailtest2) ;=> <nonterminating>

(define (tailtest3)
  ; test repeated push of empty subcontinuation
  (let ([p (newPrompt)])
    (pushPrompt p
      (withSubCont p
        (lambda (s)
          (let loop ()
            (pushSubCont s (loop))))))))

(tailtest3) ;=> <nonterminating>

(define (tailtest4)
  ; make sure we can do some other control operations and return
  ; from them in between tail calls
  (let loop ()
    (let ([p (newPrompt)])
      (pushPrompt p
        (pushPrompt p 'prompt)
        (withSubCont p
          (lambda (s)
            (pushSubCont s 'subcont)
            (pushSubCont s (loop))))))))

(tailtest4) ;=> <nonterminating>

;;; shift and reset (Danvy and Filinski 1990)
;;; requires lib.ss

(let ()
  (define emit
    (lambda (n)
      (shift c (cons n (c '*)))))
  (reset (emit 1) (emit 2) (emit 3) '())) ;=> (1 2 3)

;;; spawn (Hieb and Dybvig 1990)
;;; requires lib.ss

(spawn (lambda (c) (* 2 (c (lambda (f) (* 5 (f 3))))))) ;=> 30

(spawn (lambda (c) (* 2 (c (lambda (f) (f (f 3))))))) ;=> 12

((spawn (lambda (c) (* 2 (c (lambda (f) f))))) 3) ;=> 6

(spawn (lambda (c1)
         (* 5 ((spawn (lambda (c2)
                        (* 2 (c2 (lambda (f2)
                                   (lambda ()
                                     (* 3 (c1 (lambda (f3)
                                                7))))))))))))) ;=> 7

(spawn (lambda (c)
         (* 5 ((lambda (x) (c (lambda (f1) x)))
               (* 3 (c (lambda (f2) (* 2 (f2 7))))))))) ;=> 42

((spawn (lambda (c1)
          (* 5 (spawn (lambda (c2)
                        (* 2 ((c1 (lambda (f1)
                                    (lambda (g) (g f1 c2)))))))))))
 (lambda (f1 c2) (f1 (lambda () (* 7 (c2 (lambda (f2)
                                           (* 11 (f2 13))))))))) ;=> 10010

;; test tail recursion
(let f ()
  (spawn (lambda (c) (c (lambda (k) (f)))))) ;=> <nonterminating>

;; test tail recursion
(spawn
  (rec f
    (lambda (c)
      ((c (lambda (k)
            (k (lambda () (f c))))))))) ;=> <nonterminating>

;; exercise "no prompt found" error
(spawn (lambda (c1)
         (* 5 (spawn (lambda (c2)
                       (* 2 (c2 (lambda (f2)
                                  (* 3 (c2 (lambda (f3)
                                             7))))))))))) ;=> <error>

;; exercise "no prompt found" error
(spawn (lambda (c1)
         (* 5 ((spawn (lambda (c2)
                        (* 2 (c2 (lambda (f2)
                                   (lambda ()
                                     (* 3 (c2 (lambda (f3)
                                                7))))))))))))) ;=> <error>
