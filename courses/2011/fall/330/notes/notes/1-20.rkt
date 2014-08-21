#lang plai
(halt-on-errors)

;; abstract syntax
(define-type AE
  [setid (var symbol?)
         (val-e AE?)]
  
  [newbox (ve AE?)]
  [openbox (be AE?)]
  [beatbox (be AE?) (ve AE?)]
  [seqn (ige AE?) (ve AE?)]  
  
  [num (n number?)]
  [binop (op procedure?)
         (lhs AE?)
         (rhs AE?)]
  [if0 (cond-e AE?)
       (true-e AE?)
       (false-e AE?)]
  [id (s symbol?)]
  [fun (arg-name symbol?)
       (body AE?)]
  [ref-fun (arg-name symbol?)
           (body AE?)]
  [app (fun-expr AE?) 
       (arg AE?)])

(define (rec name named-value body)
  ;; make-the-recursive-bro!-stx = the Y (combinator)
  (define make-the-recursive-bro!-stx
    (parse '(fun (the-real-work)
                 (with (f
                        (fun (f)
                             (the-real-work
                              (fun (n)
                                   ((f f) n)))))
                       (fun (n)
                            ((f f) n))))))
  (with name
        (app
         make-the-recursive-bro!-stx    
         (fun name
              named-value))
        body))
  
(define (with name named-thing body)
  (app (fun name body) named-thing))
(define (add lhs rhs)
  (binop + lhs rhs))
(define (mult lhs rhs)
  (binop * lhs rhs))

;; An environment or Deferred Substitution
(define-type DefrdSubst
  [mtEnv]
  [anEnv (name symbol?)
        (loc number?)
        (all-the-others DefrdSubst?)])

(define-type Store
  [mtStore]
  [aStore (loc number?)
          (named-value AEV?)
          (all-the-others Store?)])

(define-type AEV
  [numV (n number?)]
  [boxV (loc number?)]
  [closureV (name symbol?)
        (body AE?)
        (env DefrdSubst?)]
  [ref-closureV (name symbol?)
        (body AE?)
        (env DefrdSubst?)])

;; concrete syntax
#|
AE = <number>
   | (setid <id> <AE>)
   | (newbox <AE>)
   | (openbox <AE>)
   | (beatbox <AE> <AE>)
   | (seqn <AE> <AE>)
   | (* <AE> <AE>)
   | (+ <AE> <AE>)
   | (- <AE> <AE>)
   | (with (<id> <AE>) <AE>)
   | (rec (<id> <AE>) <AE>)
   | (fun (<id>) <AE>)
   | (ref-fun (<id>) <AE>)
   | <id>
   | (<AE> <AE>)
   | (if0 <AE> <AE> <AE>)
|#

;; parse : concrete -> abstract
(define (parse c)
  (cond
   [(number? c)
    (num c)]   
   [(and (list? c)
         (= 3 (length c))
         (equal? 'setid (first c)))
    (setid (second c)
         (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? '+ (first c)))
    (add (parse (second c))
         (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? 'seqn (first c)))
    (seqn (parse (second c))
         (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? 'beatbox (first c)))
    (beatbox (parse (second c))
         (parse (third c)))]
   [(and (list? c)
         (= 2 (length c))
         (equal? 'newbox (first c)))
    (newbox (parse (second c)))]
   [(and (list? c)
         (= 2 (length c))
         (equal? 'openbox (first c)))
    (openbox (parse (second c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? '- (first c)))
    (binop -
           (parse (second c))
           (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? '* (first c)))
    (mult (parse (second c))
          (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? 'with (first c)))
    (with (first (second c))
          (parse (second (second c)))
          (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? 'rec (first c)))
    (rec (first (second c))
          (parse (second (second c)))
          (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? 'fun (first c)))
    (fun (first (second c))
         (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? 'ref-fun (first c)))
    (ref-fun (first (second c))
             (parse (third c)))]
   [(and (list? c)
         (= 4 (length c))
         (equal? 'if0 (first c)))
    (if0 (parse (second c))
         (parse (third c))
         (parse (fourth c)))]
   [(and (list? c)
         (= 2 (length c)))
    (app (parse (first c))
         (parse (second c)))]
   [(symbol? c)
    (id c)]
   [else
    (error 'parse "Bad programmer, no cake ~e" c)]))

(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
(test/exn (parse '(+ 1 1 1))
          "no cake")
(test (parse '(* 3 1))
      (mult (num 3) (num 1)))
(test (parse '(fun (x) x))
      (fun 'x (id 'x)))
(test (parse '((fun (x) x) 5))
      (app (fun 'x (id 'x)) (num 5)))

;; lookup-binding : symbol? DefrdSub -> AE?
(define (lookup-binding s ds)
  (type-case
   DefrdSubst ds
   [mtEnv ()
          (error 'lookup-binding "Unbound identifier ~e" s)]
   [anEnv (name named-value more)
         (if (symbol=? s name)
             named-value
             (lookup-binding s more))]))

(define (lookup-loc x st)
  (type-case
   Store st
   [mtStore ()
          (error 'lookup-loc "Unallocated address ~e" x)]
   [aStore (loc val more)
         (if (= loc x)
             val
             (lookup-loc x more))]))

(define (size st)
  (type-case
   Store st
   [mtStore ()
          0]
   [aStore (loc val more)
           (max loc (size more))]))

(define (malloc st)
  (add1 (size st)))

(define (lifted op . args)
  (apply op (map denum args)))

(define (denum v)
  (type-case
   AEV v
   [numV (n)
         n]
   [else
    (error 'interp "Not a number")]))

(define (lift binop lhs rhs)
  (numV (lifted binop lhs rhs)))

;; interp* : AE DefrdSub Store -> meaning Store
(define (interp* some-ae ds st)
  (type-case
   AE some-ae
    [num (n)
        (values (numV n)
                st)]
   [binop (op lhs rhs)
          (local [(define-values (lhs-v lhs-st) (interp* lhs ds st))
                  (define-values (rhs-v rhs-st) (interp* rhs ds lhs-st))]
                 (values
                  (lift op
                        lhs-v
                        rhs-v)
                  rhs-st))]
   [id (s)
       (values (lookup-loc (lookup-binding s ds) st)
               st)]
   [if0 (cond-e true-e false-e)
        (local [(define-values (cond-v cond-st) (interp* cond-e ds st))]
               (if (lifted zero? cond-v)
                   (interp* true-e ds cond-st)
                   (interp* false-e ds cond-st)))]
   [fun (arg-name body-expr)
        ;; Save the Environment
        ;; Create A Closure, Today!
        (values (closureV arg-name body-expr ds)
                st)]
   [ref-fun (arg-name body-expr)
        (values (ref-closureV arg-name body-expr ds)
                st)]
   [app (fun-expr arg-expr)
        ;; interp* : AE ds -> number
        ;; fun-expr : AE
        (local [(define-values (fun-v fun-st) (interp* fun-expr ds st))]
               (type-case
                AEV fun-v
                [closureV (arg-name fun-body saved-env)
                          (local [(define-values (arg-v arg-st) (interp* arg-expr ds fun-st))
                                  (define arg-loc (malloc arg-st))]
                                 (interp* fun-body
                                          (anEnv arg-name
                                                 arg-loc
                                                 saved-env)
                                          (aStore arg-loc
                                                  arg-v
                                                  arg-st)))]
                [ref-closureV (arg-name fun-body saved-env)
                              (local [(define arg-loc (lookup-binding (id-s arg-expr) ds))]
                                     (interp* fun-body
                                              (anEnv arg-name
                                                     arg-loc
                                                     saved-env)
                                              fun-st))]
                [else
                 (error 'interp "Not a function")]))]
   [seqn (fst snd)
         (local [(define-values (fst-v fst-st) (interp* fst ds st))]
                (interp* snd ds fst-st))]
   [newbox (init-val-e)
           (local [(define-values (iv-v iv-st) (interp* init-val-e ds st))
                   (define new-loc (malloc iv-st))]
                  (values (boxV new-loc)
                          (aStore new-loc
                                  iv-v
                                  iv-st)))]
   [openbox (box-e)
            (local [(define-values (box-v box-st) (interp* box-e ds st))]
                   (type-case
                    AEV box-v
                    [boxV (box-loc)
                          (values (lookup-loc box-loc box-st)
                                  box-st)]
                    [else
                     (error 'interp "Not a box")]))]
   [beatbox (box-e new-e)
            (local [(define-values (box-v box-st) (interp* box-e ds st))
                    (define-values (new-v new-st) (interp* new-e ds box-st))]
                   (type-case
                    AEV box-v
                    [boxV (box-loc)
                          (values new-v
                                  (aStore box-loc
                                          new-v
                                          new-st))]
                    [else
                     (error 'interp "Not a box")]))]
   [setid (lvalue val-e)
        (local [(define-values (val-v val-st) (interp* val-e ds st))]
               (values val-v
                       (aStore (lookup-binding lvalue ds)
                               val-v
                               val-st)))]))

;; Racket
;;(set! x 5)

;;Common Lisp
;;(setq x 5) ;; OK
;;(setq (+ x 5) 10) ;; BAD
;;(setq (first x) 11) ;; OK
;;(setq (first (rest (first x))) 12) ;; OK
;;(define (second l) (first (rest l)))
;;(setq (second x) 12) ;; BAD

(define (interp ae)
  (define-values (ans final-st)
    (interp* ae (mtEnv) (mtStore)))
  (printf "The final store ~v\n" final-st)
  (type-case
   AEV ans
   [numV (n) n]
   [else ans]))

(test/exn (interp (parse '(5 4)))
          "Not a function")
(test/exn (interp (parse '(+ (fun (x) x) 1)))
          "Not a number")      

(test (interp (parse '(fun (x) x)))
      (closureV 'x (id 'x) (mtEnv)))

(test (interp (parse '5))
      5)
(test (interp (parse '42))
      42)

(test (interp (parse '(+ 1 1)))
      2)
(test (interp (parse '(+ 1 99)))
      100)

(test (interp (parse '(* 3 2)))
      6)
(test (interp (parse '(* 1/99 99)))
      1)

(test (interp (parse '(+ (+ (+ (+ 1 1) (+ 1 1)) (+ (+ 1 1) (+ 1 1))) (+ 1 1))))
      10)

(test (interp (parse '(with (x (+ 1 1)) (+ x x))))
      4)
(test (interp (parse '(with (x 2) (+ x x))))
      4)
(test (interp (parse '(with (x 2) (+ 2 x))))
      4)
(test (interp (parse '(with (x 2) (+ 2 2))))
      4)
(test (interp (parse '(+ 2 2)))
      4)

(test/exn (interp (parse 'x))
          "Unbound")
(test (interp (parse '(with (x (+ 1 1)) (* x x))))
      4)
(test (interp (parse '(with (x 1) (with (y 2) (+ x y)))))
      3)
(test (interp (parse '(with (x 1) (with (y x) (+ x y)))))
      2)
(test (interp (parse '(with (x 1) (with (x x) (+ x x)))))
      2)

(test (interp (parse '(with (y 2) (+ 1 y))))
      3)

;; This tells if we are substituting text or not:
(test/exn (interp (parse '(with (y x) 3)))
          "Unbound")

(test (parse '(foo 1)) (app (id 'foo) (num 1)))
(test (interp (parse '(with (double (fun (x) (+ x x)))
                            (double 3))))
      6)
(test (interp (parse '(with (double (fun (x) (+ x x)))
                            (double (+ 3 2)))))
      10)

(test (interp (parse '(with (g 10)
                            (with (f (fun (n) (+ g 5)))
                                  (f 5)))))
      15)
(test (interp (parse '(with (g (fun (m) (+ m 1)))
                            (with (f (fun (n) (g (+ n 5))))
                                  (f 5)))))
      11)

;(test (interp (parse '(f 5)) (list (fundef 'f 'n (app 'f (add (id 'n) (num 5))))))
;      11)

;; induction vs co-induction
;; recursion vs co-recursion

;; Lisp1 vs (we are Lisp2)

(test (interp (parse '(with (x 5)
                            (+ (+ x x)
                               (* x x)))))
      35)

(test (interp (parse
               ;; ds = mt
               '(with (x 5)
                      ;; ds = x -> 5 :: mt
                      (+
                       ;; ds = x -> 5 :: mt
                       (with (x 10)
                             ;; ds = x -> 10 :: x -> 5 :: mt
                             (+ x x))
                       ;; ds = x -> 5 :: mt
                       (* x x)))))
      45)

(test (interp (parse
               '(with (x 5)
                      (+
                       (with (x x)
                             (+ x x))
                       (* x x)))))
      35)

(test (interp (parse '(+ (with (x 5)
                               x)
                         (with (x 7)
                               x))))
      12)


;; f(n) = g(n+5)
;; g(m) = n+1
;; f(5)
(test/exn (interp (parse '(with (g (fun (m) (+ n 1)))
                                (g 5))))
          "Unbound identifier")
(test/exn (interp (parse '(with (g (fun (m) (+ n 1)))
                                (with (n 10) (g 5)))))
          "Unbound identifier")

(test (interp (parse '(with (x 5) (+ x x))))
      10)

;; f(x) = x + x; f(5)
;; Voldemort(x) = x+x; Voldemort(5)
;; (\ x. x + x) 5
(test (interp (parse '((fun (x) (+ x x)) 5)))
      10)

(test (interp (parse '(if0 0 1 2)))
      1)
(test (interp (parse '(if0 1 1 2)))
      2)

(test/exn (interp (parse '(with (fac
                                 (fun (n)
                                      (if0 n
                                           1
                                           (* n (fac (- n 1))))))
                                (fac 5))))
          "Unbound identifier")

(test (interp (parse '(rec (fac
                             (fun (n)
                                  (if0 n
                                       1
                                       (* n (fac (- n 1))))))
                            (fac 0))))
      1)
(test (interp (parse '(rec (fac
                             (fun (n)
                                  (if0 n
                                       1
                                       (* n (fac (- n 1))))))
                           (fac 1))))
      1)
(test (interp (parse '(rec (fac
                             (fun (n)
                                  (if0 n
                                       1
                                       (* n (fac (- n 1))))))
                           (fac 2))))
      2)
(test (interp (parse '(rec (fac
                             (fun (n)
                                  (if0 n
                                       1
                                       (* n (fac (- n 1))))))
                            (fac 5))))
      120)

;;(test (interp (parse '(rec (x (+ x 2))
;;                           x)))
;;      2.145834543)

(test (interp (parse '(fun (x) (x x))))
      (closureV 'x (app (id 'x) (id 'x)) (mtEnv)))

(test/exn (interp (parse '(with (o (fun (x) (x x)))
                                (o (fun (n) (+ n 1))))))
          "Not a number")
(test/exn (interp (parse '((fun (n) (+ n 1))
                           (fun (n) (+ n 1)))))
          "Not a number")

(test/exn (interp (parse '(+ 
                           (fun (n) (+ n 1))
                           1)))
          "Not a number")
      
;;(test (interp (parse '(with (o (fun (x) (x x)))
;;                            (o o))))
;;      (interp (parse
;;               '(with (o (fun (x) (x x)))
;;                      (o o)))))


(test (interp (parse '(with (fac
                             (fun (fac)
                                  (fun (n)
                                       (if0 n
                                            1
                                            (* n ((fac fac) (- n 1)))))))
                            ((fac fac) 5))))
      120)

(define (fib n)
  (cond
   [(= n 0) 1]
   [(= n 1) 1]
   [else
    (+ (fib (- n 1))
       (fib (- n 2)))]))

(test (interp (parse '(with (fib
                             (fun (fib)
                                  (fun (n)
                                       (if0 n
                                            1
                                            (if0 (- n 1)
                                                 1
                                                 (+ ((fib fib) (- n 1))
                                                    ((fib fib) (- n 2))))))))
                            ((fib fib) 5))))
      (fib 5))

(test (interp (parse '(with (make-this-recursive-bro!
                             (fun (the-real-work)
                                  (with (f
                                         (fun (f)
                                              (the-real-work
                                               (fun (n)
                                                    ((f f) n)))))
                                        (fun (n)
                                             ((f f) n)))))
                            (with (fac
                                   (make-this-recursive-bro!
                                    (fun (fac)
                                         (fun (n)
                                              (if0 n
                                                   1
                                                   (* n (fac (- n 1))))))))
                                  (fac 5)))))
      120)

;; Mutation

#|
 int x = 3;

 .... 10,000 lines of code ...

 if ( somegiantcomputation() )
  x = 4

 .... 10,000 lines of code ...

 *( srand()*4096 ) = 12;

 Point* p = new Point( 5, 5 );

 *(p + sizeof(int)) = 10;

 return x + 10;
|#

#|
 x = 3
 ...
 ...
 ...
 x + 10
|#

#|
(define (f x)
  (+ x 10))

'user-input = (list "d" "o" "c" "t" "o" "r" "h" "e" "l" "p" "m" "e" "i" "a" "m" "t" "r" "a" "p" "p" "e" "d" "inaninfinitelist" )

(define x (ht))
(define after-y (hash-set x 'y 1))
(define after-y2 (hash-set after-y 'y 2))
|#

;; Functional = Persistent

(test (interp (parse '(newbox 1)))
      (boxV 1))
(test/exn (interp (parse '(+ (newbox 1) 5)))
          "Not a number")
(test/exn (interp  (parse '(openbox 42)))
          "Not a box")
(test (interp (parse '(openbox (newbox 1))))
      1)
(test (interp (parse '(beatbox (newbox 1) 2)))
      ;; WWCD... if (x = 4) { ... } else { ... }
      2)
(test (interp (parse '(seqn (beatbox (newbox 1) 2)
                             (openbox (newbox 1)))))
      1)
(test (interp (parse '(with (thisGuy (newbox 1))
                             (seqn (beatbox thisGuy 2)
                                   (openbox thisGuy)))))
      2)
(test (interp (parse '(with (thisGuy (newbox 1))
                            (+ (openbox thisGuy)
                               (beatbox thisGuy 2)))))
      3)
(test (interp (parse '(with (thisGuy (newbox 1))
                            (+ (beatbox thisGuy 2)
                               (openbox thisGuy)))))
      4)

(test (interp (parse '(with (swap
                             (fun (xb)
                                  (fun (yb)
                                       (with [tmp (openbox xb)]
                                             (seqn (beatbox xb (openbox yb))
                                                   (beatbox yb tmp))))))
                            (with (harry-box (newbox 9))
                                  (with (kwisatz-haderach (newbox 42))
                                        (seqn ((swap harry-box) kwisatz-haderach)
                                              (openbox harry-box)))))))
      42)
                

;; scope is when you can name things
;; extent is how long they exist

;; scope = extent
;;(fun (y)
;;     (newbox (& y)))

(test (interp (parse '(with (z 5)
                            (+ z
                               (seqn (setid z 12)
                                     z)))))
      17)
(test (interp (parse '(with (z 5)
                            (+ (seqn (setid z 12)
                                     z)
                               z))))
      24)

(test (interp (parse '(with (swap
                             (fun (x)
                                  (fun (y)
                                       (with [tmp x]
                                             (seqn (setid x y)
                                                   (setid y tmp))))))
                            (with (harry 9)
                                  (with (kwisatz-haderach 42)
                                        (seqn ((swap harry) kwisatz-haderach)
                                              harry))))))
      9)

(test (interp (parse '(with (swap
                             (ref-fun (x)
                                  (ref-fun (y)
                                       (with [tmp x]
                                             (seqn (setid x y)
                                                   (setid y tmp))))))
                            (with (harry 9)
                                  (with (kwisatz-haderach 42)
                                        (seqn ((swap harry) kwisatz-haderach)
                                              harry))))))
      42)

;; Hey man, gimme /
;; Here's /
;; Closes

;; Hey man, gimme /cutegirls.jpg
;; Hey man, it's BYU
;; Closes

#|
int x = 12;
Req* r = readreq();
x = r->awesomeness();
sendback(ans); === exit(0)
Req* r1 = readreq();
sendback(ans2);
|#

#|
 STACKY

 local variables
 ...
 return pointer
 argument1
 argument2
 ...

|#
