#lang plai
(halt-on-errors)

;; XXX remove envs
;; XXX remove closures/numVs
;; XXX meta interpreter
;; XXX meta-circular
;; XXX encode booleans
;; XXX encode lists
;; XXX encode numbers

;; abstract syntax
(define-type AE
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

;; concrete syntax
#|
AE = <number>
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

;; An environment or Deferred Substitution

;; DefrdSubst = symbol? -> AEV?
(define DefrdSubst? procedure?)

(define (mtEnv)
  (lambda (s)
    (error 'lookup-binding "Unbound identifier ~e" s)))
(define (anEnv name named-value all-the-others)
  (lambda (s)
    (if (equal? name s)
        named-value
        (all-the-others s))))

;; lookup-binding : symbol? DefrdSub -> AE?
(define (lookup-binding s ds)
  (ds s))

(define (AEV? x)
  (or (number? x)
      (procedure? x)))

(define (numV n)
  n)
(define (closureV name body env)
  (lambda (value-of-name)
    (interp* body
             (anEnv name
                    value-of-name
                    env))))

(define (lifted op . args)
  (apply op (map denum args)))

(define (denum v)
  (if (number? v)
      v
      (error 'interp "Not a number")))

(define (lift binop lhs rhs)
  (numV (lifted binop lhs rhs)))

;; interp* : AE DefrdSub -> meaning
(define (interp* some-ae ds)
  (type-case
   AE some-ae
   [num (n)
        (numV n)]
   [binop (op lhs rhs)
          (lift op
                (interp* lhs ds)
                (interp* rhs ds))]
   [id (s)
       (lookup-binding s ds)]
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
        (local [(define fun-val (interp* fun-expr ds))]
               (if (procedure? fun-val)
                   (fun-val (interp* arg-expr ds))
                   (error 'interp "Not a function")))]))

(define (interp ae)
  (define ans
    (interp* ae (mtEnv)))
  ans)

(test/exn (interp (parse '(5 4)))
          "Not a function")
(test/exn (interp (parse '(+ (fun (x) x) 1)))
          "Not a number")      

;;(test (interp (parse '(fun (x) x)))
;;      (closureV 'x (id 'x) (mtEnv)))

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

;;(test (interp (parse '(fun (x) (x x))))
;;      (closureV 'x (app (id 'x) (id 'x)) (mtEnv)))

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

;; Church Encoding

;; Turing ... Church.... Thesis???

;; Alan Turing and Alonzo Church ... Church-Turing Thesis
;;   ====> "anything that can be solved effectively can be solved on a Turning Machine or with the Lambda Calculus"
;;   ====> effective mathetics

;; Lambda Calculus has exprs e

;; e = x
;;   | e e
;;   | \x.e

;; (\x.e) e' => e [x <- e']

;; Booleans
(define lc:true
  (lambda (opt1)
    (lambda (opt2)
      opt1)))
(define lc:false
  (lambda (opt1)
    (lambda (opt2)
      opt2)))

(test ((lc:true 0) 1)
      0)
(test ((lc:false 0) 1)
      1)

(define lc:and
  (lambda (fst-bool)
    (lambda (snd-bool)
      (lambda (opt1)
        (lambda (opt2)
          (((fst-bool (lambda (x)
                        ((snd-bool opt1) opt2)))
            (lambda (x) opt2))
           (lambda (x) x)))))))

(test ((((lc:and lc:true) lc:true) 0) 1)
      0)
(test ((((lc:and lc:false) lc:true) 0) 1)
      1)
(test ((((lc:and lc:true) lc:false) 0) 1)
      1)
(test ((((lc:and lc:false) lc:false) 0) 1)
      1)

;; Natural Number
(define lc:zero
  (lambda (f)
    (lambda (x)
      x)))
(define lc:one
  (lambda (f)
    (lambda (x)
      (f x))))
(define lc:two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(test ((lc:zero add1) 0)
      0)
(test ((lc:one add1) 0)
      1)
(test ((lc:two add1) 0)
      2)

(define lc:succ
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

(test (((lc:succ lc:zero) add1) 0)
      1)
(test (((lc:succ lc:one) add1) 0)
      2)
(test (((lc:succ lc:two) add1) 0)
      3)

(define lc:plus
  (lambda (n)
    (lambda (m)
      (lambda (f)
        (lambda (x)
          ((n f) ((m f) x)))))))

(test ((((lc:plus lc:two) lc:two) add1) 0)
      4)
(test ((((lc:plus (lc:succ lc:two)) lc:two) add1) 0)
      5)

(define lc:mult
  (lambda (n)
    (lambda (m)
      (lambda (f)
        (n (m f))))))

(test ((((lc:mult lc:two) lc:two) add1) 0)
      4)
(test ((((lc:mult (lc:succ lc:two)) lc:two) add1) 0)
      6)

;; Dedkind cut

(define lc:cons
  (lambda (fst)
    (lambda (rst)
      (lambda (sel)
        ((sel fst) rst)))))
(define lc:first
  (lambda (fst)
    (lambda (rst)
      fst)))
(define lc:rest
  (lambda (fst)
    (lambda (rst)
      rst)))

(test (((lc:cons 1) 2) lc:first)
      1)
(test (((lc:cons 1) 2) lc:rest)
      2)
