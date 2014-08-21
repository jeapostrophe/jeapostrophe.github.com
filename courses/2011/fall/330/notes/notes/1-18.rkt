#lang plai
(halt-on-errors)

;; abstract syntax
(define-type AE
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
        (named-value AEV?)
        (all-the-others DefrdSubst?)])

(define-type AEV
  [numV (n number?)]
  [closureV (name symbol?)
        (body AE?)
        (env DefrdSubst?)])

;; concrete syntax
#|
AE = <number>
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
        (values (numV n) st)]
   [binop (op lhs rhs)
          (local [(define-values (lhs-v lhs-st) (interp* lhs ds st))
                  (define-values (rhs-v rhs-st) (interp* rhs ds lhs-st))]
                 (values
                  (lift op
                        lhs-v
                        rhs-v)
                  rhs-st))]
   [id (s)
       (values (lookup-binding s ds) st)]
   ;; XXX continue next class from here...
   [if0 (cond-e true-e false-e)
        (if (lifted zero? (interp* cond-e ds))
            (interp* true-e ds)
            (interp* false-e ds))]
   [fun (arg-name body-expr)
        ;; Save the Environment
        ;; Create A Closure, Today!
        (closureV arg-name body-expr ds)]
   [app (fun-expr arg-expr)
        ;; interp* : AE ds -> number
        ;; fun-expr : AE
        (type-case
         AEV (interp* fun-expr ds)
         [closureV (arg-name fun-body saved-env)
              (interp* fun-body
                       (anEnv arg-name
                             (interp* arg-expr ds)
                             ;; Evil = Dynamic Scope
                             ;ds
                             ;; Correct = Static Scope
                             saved-env))]
         [else
          (error 'interp "Not a function")])]
   [else
    (error 'interp "Ain't no boxes")]

                             ))

(define (interp ae)
  (define ans
    (interp* ae (mtEnv)))
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

(define (boxV v) (error 'boxV))

(test (interp (parse '(newbox 1)))
      (boxV (numV 1)))
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



                
