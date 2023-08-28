#lang plai
(halt-on-errors)

;; abstract syntax
(define-type AE
  [setid (id symbol?)
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
  [refun (arg-name symbol?)
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

(define-type Memory
  [mtMem]
  [anMem (loc number?)
         (val AEV?)
         (all-the-others Memory?)])

(define-type AEV
  [numV (n number?)]
  [boxV (loc number?)]
  [closureV (name symbol?)
            (body AE?)
            (env DefrdSubst?)]
  [reclosureV (name symbol?)
              (body AE?)
              (env DefrdSubst?)])

;; concrete syntax
#|
AE = <number>
   | (setid <id> <AE>)
   | (* <AE> <AE>)
   | (+ <AE> <AE>)
   | (- <AE> <AE>)
   | (with (<id> <AE>) <AE>)
   | (rec (<id> <AE>) <AE>)
   | (fun (<id>) <AE>)
   | (refun (<id>) <AE>)
   | <id>
   | (<AE> <AE>)
   | (if0 <AE> <AE> <AE>)

   | (newbox <AE>)
   | (openbox <AE>)
   | (beatbox <AE> <AE>)
   | (seqn <AE> <AE>)
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
         (= 3 (length c))
         (equal? 'refun (first c)))
    (refun (first (second c))
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

(define (lookup-addr l mem)
  (type-case
   Memory mem
   [mtMem ()
          (error 'lookup-addr "Segmentation fault ~e" l)]
   [anMem (ol v more)
          (if (= l ol)
             v
             (lookup-addr l more))]))

(define (biggest mem)
  (type-case
   Memory mem
   [mtMem () 0]
   [anMem (ol v more)
          (max ol (biggest more))]))

(define (malloc mem)
  (add1 (biggest mem)))

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

;; interp* : AE DefrdSub Memory -> meaning Memory
(define (interp* some-ae ds mem)
  (type-case
   AE some-ae
   [num (n)
        (values (numV n)
                mem)]
   [binop (op lhs rhs)
          (local [(define-values (lhs-v lhs-mem) (interp* lhs ds mem))
                  (define-values (rhs-v rhs-mem) (interp* rhs ds lhs-mem))]

                 (values
                  (lift op
                        ;; lhs = (beatbox thisGuy 2), [ds => thisGuy : somebox (1)]
                        lhs-v
                        ;; rhs = (openbox thisGuy), [ds => thisGuy : somebox (1)]
                        rhs-v)
                  rhs-mem))]
   [id (s)
       (values (lookup-addr (lookup-binding s ds) mem)
               mem)]
   [if0 (cond-e true-e false-e)
        (local [(define-values (cond-v cond-mem) (interp* cond-e ds mem))]
               (if (lifted zero? cond-v)
                   (interp* true-e ds cond-mem)
                   (interp* false-e ds cond-mem)))]
   [fun (arg-name body-expr)
        (values (closureV arg-name body-expr ds)
                mem)]
   [refun (arg-name body-expr)
          (values (reclosureV arg-name body-expr ds)
                  mem)]
   [app (fun-expr arg-expr)
        ;; interp* : AE ds -> number
        ;; fun-expr : AE        
        (local [(define-values (fun-v fun-mem) (interp* fun-expr ds mem))]
               (type-case
                AEV fun-v
                [closureV (arg-name fun-body saved-env)
                          (local [(define-values (arg-v arg-mem) (interp* arg-expr ds fun-mem))
                                  (define arg-loc (malloc arg-mem))]
                                 (interp* fun-body
                                          (anEnv arg-name
                                                 arg-loc
                                                 ;; Evil = Dynamic Scope
                                                 ;;ds
                                                 ;; Correct = Static Scope
                                                 saved-env)
                                          (anMem arg-loc
                                                 arg-v
                                                 arg-mem)))]
                [reclosureV (arg-name fun-body saved-env)
                            (local [(define arg-loc (lookup-binding (id-s arg-expr) ds))]
                                   (interp* fun-body
                                            (anEnv arg-name
                                                   arg-loc
                                                   saved-env)
                                            mem))]
                [else
                 (error 'interp "Not a function")]))]
   [seqn (fst snd)
         (local [(define-values (fst-v fst-mem) (interp* fst ds mem))]
                (interp* snd ds fst-mem))]
   [newbox (init-e)
           (local [(define-values (init-v init-mem) (interp* init-e ds mem))
                   (define new-loc (malloc init-mem))]
                  (values (boxV new-loc)
                          (anMem new-loc
                                 init-v
                                 init-mem)))]
   [openbox (box-e)
            (local [(define-values (box-v box-mem) (interp* box-e ds mem))]
                   (type-case
                    AEV box-v
                    [boxV (box-addr)
                          (values (lookup-addr box-addr box-mem)
                                  box-mem)]
                    [else
                     (error 'interp "Not a box")]))]
   [beatbox (box-e val-e)
            (local [(define-values (box-v box-mem) (interp* box-e ds mem))
                    (define-values (val-v val-mem) (interp* val-e ds box-mem))]
                   (type-case
                    AEV box-v
                    [boxV (box-addr)
                          (values val-v
                                  (anMem box-addr
                                         val-v
                                         val-mem))]
                    [else
                     (error 'interp "Not a box")]))]
   [setid (some-id val-e)
          (local [(define-values (val-v val-mem) (interp* val-e ds mem))]
                 (values val-v
                         (anMem (lookup-binding some-id ds)
                                val-v
                                val-mem)))]     
   ))

(define (interp ae)
  (define-values (ans fin-mem)
    (interp* ae (mtEnv) (mtMem)))
  (printf "The final memory is ~v\n" fin-mem)
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

;;(with (x 1)
;;      (+ ... 
;;         (+ 5 x)))

;; Mutation

#|
 int x = 3;

 ... 2000 lines ...
 x = 4;
 ... 2000 lines ...
 // x = 77

 int evil = 42;
 scanf("%d", &evil);
 *evil = 12;

 strcpy(&usersbuffer, &mybuffer)

 Point* p = new Point( 5, 10 );
 p->x /// hey thats private
 printf("%d", *p)
 *p = 10
 *(p + sizeof(int)) = 15

 return x + 10;
|#

;; y = y + 1

#|
Time 1
 a = 4,4
 b = 5,5

Time 2
 a' = 6,6
 b' = f(a,b) != f(a',b)
|#

(test (interp (parse '(openbox (newbox 1))))
      1)
(test/exn (interp (parse '(openbox 42)))
          "Not a box")
(test/exn (interp (parse '(+ 1 (newbox 42))))
          "Not a number")
(test (interp (parse '(beatbox (newbox 1) 2)))

      #|
      int x = 1;
      int y = 2;
      y = (x = 4) + (x = x + 1);
      printf("%d, %d\n", x, y)
      |#
      
      ;; WWCD = if ( x = 4 ) { printf "It's four\n" } else { printf "Not four\n" }
      ;;A box with 2 in it
      ;;1
      2)

(test (interp (parse '(seqn (beatbox (newbox 1) 2)
                            (openbox (newbox 1)))))
      1)
(test (interp (parse '(with (evilBox (newbox 1))
                            (seqn (beatbox evilBox 2)
                                  (openbox evilBox)))))
      2)
(test (interp (parse '(with (evilBox (newbox 1))
                            (+ (beatbox evilBox 2)
                               (openbox evilBox)))))
      4)

#|
C without pointer arithmetic:

int x = 4;
f(x++);
return x + 3;
|#

#|
Lazy C without pointer arithmetic:

int x = 4;
f(x++);
return x + 3;

int f ( int y ) {
 return 4;
}

int f ( int y ) {
 return 5 + y;
}
|#

(test (interp (parse '(with (swap
                             (fun (xb)
                                  (fun (yb)
                                       (with [tmp (openbox xb)]
                                             (seqn (beatbox xb (openbox yb))
                                                   (beatbox yb tmp))))))
                            (with (harry-box (newbox 7))
                                  (with (kwisatz-haderach (newbox 42))
                                        (seqn ((swap harry-box) kwisatz-haderach)
                                              (openbox harry-box)))))))
      42)

;; scope = extent
;; scope is where the name makes sense
;; extent is where the value makes sense

;;(fun (x)
;;     (box (& x)))

(test (interp (parse '(with [t 7]
                            (+ t
                               (seqn (setid t 12)
                                     t)))))
      19)
(test (interp (parse '(with [t 7]
                            (+ t
                               (with [t 12]
                                     t)))))
      19)
(test (interp (parse '(with [t 7]
                            (+ (seqn (setid t 12)
                                     t)
                               t))))
      24)

(test (interp (parse '(with (swap
                             (fun (x)
                                  (fun (y)
                                       (with [tmp x]
                                             (seqn (setid x y)
                                                   (setid y tmp))))))
                            (with (harry 7)
                                  (with (kwisatz-haderach 42)
                                        (seqn ((swap harry) kwisatz-haderach)
                                              harry))))))
      7)

(test (interp (parse '(with (swap
                             (refun (x)
                                  (refun (y)
                                       (with [tmp x]
                                             (seqn (setid x y)
                                                   (setid y tmp))))))
                            (with (harry 7)
                                  (with (kwisatz-haderach 42)
                                        (seqn ((swap harry) kwisatz-haderach)
                                              harry))))))
      42)

;; HTTP session
;; GET /
;; here's the slash
;; death

;; GET /cutegirls.jpg
;; hey, it's byu
;; death

#|
int x = 12;
Req* r1 = readreq();
x = r1->awesomeness();
sendans( "Here's the x %d", x ); => exit(0)
Req* r2 = readreq();
sendans( "Here's the x %d again", x );
|#
