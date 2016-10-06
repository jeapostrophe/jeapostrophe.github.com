#lang plai-typed

;; Why did we remove &lam

(module+ main
  (define-type ExprC
    [numC (n : number)]
    [addC (l : ExprC) (r : ExprC)]
    [mulC (l : ExprC) (r : ExprC)]
    [idC (s : symbol)]
    [lamC (arg : symbol) (body : ExprC)]
    [appC (fun : ExprC) (arg : ExprC)]
    [boxC (v : ExprC)]
    [unboxC (b : ExprC)]
    [set-boxC (b : ExprC) (v : ExprC)]
    [seqC (fst : ExprC) (snd : ExprC)]
    [set-varC (s : symbol) (val : ExprC)]
    [if0C (c : ExprC) (t : ExprC) (f : ExprC)])

  (define (subC [l : ExprC] [r : ExprC]) : ExprC
    (addC l (mulC (numC -1) r)))

  (define (negC [o : ExprC]) : ExprC
    (subC (numC 0) o))

  (define (withC [name : symbol] [named-expr : ExprC] [body : ExprC]) : ExprC
    (appC (lamC name body) named-expr))

  (define-type Val
    [numV (n : number)]
    [cloV (lam : ExprC) (env : Env)]
    [boxV (ptr : Pointer)])

  (define-type Binding
    [bind (name : symbol) (ptr : Pointer)])
  (define-type-alias Env (listof Binding))
  (define mt-env empty)
  (define extend-env cons)

  (define (env-lookup [for : symbol] [env : Env]) : Pointer
    (cond
      [(empty? env) (error 'env-lookup "name not found")]
      [(symbol=? for (bind-name (first env)))
       (bind-ptr (first env))]
      [else (env-lookup for (rest env))]))

  (define-type-alias Pointer number)
  (define-type Cell
    [cell (ptr : Pointer) (val : Val)])
  (define-type-alias Memory (listof Cell))
  (define mt-mem empty)
  (define override-mem cons)

  (define (malloc [mem : Memory]) : Pointer
    (length mem))

  (define (mem-lookup [for : Pointer] [mem : Memory]) : Val
    (cond
      [(empty? mem) (error 'mem-lookup "Segmentation Fault")]
      [(= for (cell-ptr (first mem)))
       (cell-val (first mem))]
      [else (mem-lookup for (rest mem))]))

  (define (numV-binop [o : (number number -> number)] [l : Val] [r : Val]) : Val
    (numV (o (numV-n l) (numV-n r))))

  (define-type Val&Memory
    [v&m (val : Val) (mem : Memory)])

  (define (interp [expr : ExprC] [env : Env] [mem : Memory]) : Val&Memory
    (type-case
     ExprC expr
     [numC (n) (v&m (numV n) mem)]
     [addC
      (l r)
      (local [(define l-v&m (interp l env mem))
              (define r-v&m (interp r env (v&m-mem l-v&m)))]
        (v&m (numV-binop + (v&m-val l-v&m) (v&m-val r-v&m))
             (v&m-mem r-v&m)))]
     [mulC
      (l r)
      (local [(define l-v&m (interp l env mem))
              (define r-v&m (interp r env (v&m-mem l-v&m)))]
        (v&m (numV-binop * (v&m-val l-v&m) (v&m-val r-v&m))
             (v&m-mem r-v&m)))]

     [idC
      (n)
      (v&m (mem-lookup (env-lookup n env) mem)
           mem)]
     [set-varC
      (s v)
      (local [(define ptr-to-s (env-lookup s env))
              (define new-val&mem (interp v env mem))
              (define new-val (v&m-val new-val&mem))]
        (v&m new-val
             (override-mem (cell ptr-to-s new-val)
                           (v&m-mem new-val&mem))))]

     [lamC
      (arg body)
      (v&m (cloV expr env)
           mem)]
     [appC
      (f a)
      (local [(define f-v&m (interp f env mem))
              (define fd (v&m-val f-v&m))
              (define a-v&m (interp a env (v&m-mem f-v&m)))
              (define new-ptr (malloc (v&m-mem a-v&m)))]
        (interp (lamC-body (cloV-lam fd))
                (extend-env (bind (lamC-arg (cloV-lam fd))
                                  new-ptr)
                            (cloV-env fd))
                (override-mem (cell new-ptr (v&m-val a-v&m))
                              (v&m-mem a-v&m))))]

     [boxC
      (iv)
      (local [(define iv-v&m (interp iv env mem))
              (define new-ptr (malloc (v&m-mem iv-v&m)))
              (define new-mem
                (override-mem (cell new-ptr
                                    (v&m-val iv-v&m))
                              (v&m-mem iv-v&m)))]
        (v&m (boxV new-ptr)
             new-mem))]
     [unboxC
      (c)
      (local [(define c-v&m (interp c env mem))
              (define c-ptr (boxV-ptr (v&m-val c-v&m)))]
        (v&m (mem-lookup c-ptr (v&m-mem c-v&m))
             (v&m-mem c-v&m)))]
     [set-boxC
      (c nv)
      (local
          [(define b-v&m (interp c env mem))
           (define box-ptr (boxV-ptr (v&m-val b-v&m)))
           (define nv-v&m (interp nv env (v&m-mem b-v&m)))]
        (v&m (v&m-val nv-v&m)
             (override-mem (cell box-ptr (v&m-val nv-v&m))
                           (v&m-mem nv-v&m))))]
     [seqC
      (fst snd)
      (local [(define f-v&m (interp fst env mem))]
        (interp snd env (v&m-mem f-v&m)))]
     [if0C
      (c t f)
      (local [(define c-v&m (interp c env mem))]
        (if (zero? (numV-n (v&m-val c-v&m)))
          (interp t env (v&m-mem c-v&m))
          (interp f env (v&m-mem c-v&m))))]))

  (define (interp* [expr : ExprC]) : Val
    (v&m-val (interp expr mt-env mt-mem)))

  (test (interp* (if0C (numC 0) (numC 2) (numC 3)))
        (numV 2))
  (test (interp* (if0C (numC 1) (numC 2) (numC 3)))
        (numV 3))

  (test (interp* (numC 5))
        (numV 5))
  (test (interp* (numC 6))
        (numV 6))

  (test (interp* (addC (numC 5) (numC 6)))
        (numV 11))
  (test (interp* (addC (numC 5) (numC 10)))
        (numV 15))

  (test (interp* (mulC (numC 5) (numC 6)))
        (numV 30))
  (test (interp* (mulC (numC 5) (numC 10)))
        (numV 50))

  (test (interp* (subC (numC 5) (numC 6)))
        (numV -1))
  (test (interp* (subC (numC 5) (numC 10)))
        (numV -5))

  (test (interp* (negC (numC 5)))
        (numV -5))
  (test (interp* (negC (numC 10)))
        (numV -10))

  (test/exn (interp* (idC 'x))
            "not found")

  (test (interp* (lamC 'x (idC 'x)))
        (cloV (lamC 'x (idC 'x)) mt-env))

  (test (interp* (appC (lamC 'x (idC 'x)) (numC 5)))
        (numV 5))

  (test (interp* (withC 'x (numC 5) (idC 'x)))
        (numV 5))
  (test (interp* (withC 'x (numC 5)
                        (withC 'y (numC 6)
                               (idC 'x))))
        (numV 5))
  (test (interp* (withC 'x (numC 5)
                        (withC 'x (numC 6)
                               (idC 'x))))
        (numV 6))
  (test/exn (interp* (withC 'x (idC 'y)
                            (withC 'y (numC 6)
                                   (idC 'x))))
            "not found")
  (test (interp* (withC 'x (numC 5)
                        (withC 'f (lamC 'y (addC (idC 'x) (idC 'y)))
                               (appC (idC 'f) (idC 'x)))))
        (numV 10))
  (test (interp* (withC 'x (numC 5)
                        (withC 'f (lamC 'y (addC (idC 'x) (idC 'y)))
                               (withC 'x (numC 7)
                                      (appC (idC 'f) (idC 'x))))))
        (numV 12))


  ;; Delete the next line to run forever
  #;
  (test (interp* (withC 'omega (lamC 'x (appC (idC 'x) (idC 'x)))
                        (appC (idC 'omega) (idC 'omega))))
        (numV 42))

  (test (interp*
         (withC 'x (boxC (numC 5))
                (addC (unboxC (idC 'x))
                      (seqC
                       (set-boxC (idC 'x)
                                 (numC 6))
                       (unboxC (idC 'x))))))
        (numV 11))

  (test (interp* (withC 'x (numC 5)
                        (addC (idC 'x)
                              (addC (withC 'x (numC 6)
                                           (idC 'x))
                                    (idC 'x)))))
        (numV 16))

  (test (interp* (withC 'global (boxC (numC 0))
                        (withC 'counter
                               (lamC 'x
                                     (set-boxC (idC 'global)
                                               (addC (numC 1)
                                                     (unboxC (idC 'global)))))
                               (seqC (appC (idC 'counter) (numC 0))
                                     (seqC (appC (idC 'counter) (numC 0))
                                           (appC (idC 'counter) (numC 0)))))))
        (numV 3))

  (test (interp* (withC 'counter
                        (lamC 'x
                              (withC 'global (boxC (numC 0))
                                     (set-boxC (idC 'global)
                                               (addC (numC 1)
                                                     (unboxC (idC 'global))))))
                        (seqC (appC (idC 'counter) (numC 0))
                              (seqC (appC (idC 'counter) (numC 0))
                                    (appC (idC 'counter) (numC 0))))))
        (numV 1))

  (test (interp* (withC 'counter
                        (withC 'global (boxC (numC 0))
                               (lamC 'x
                                     (set-boxC (idC 'global)
                                               (addC (numC 1)
                                                     (unboxC (idC 'global))))))
                        (seqC (appC (idC 'counter) (numC 0))
                              (seqC (appC (idC 'counter) (numC 0))
                                    (appC (idC 'counter) (numC 0))))))
        (numV 3))

  (interp (withC 'counter
                 (withC 'global (boxC (numC 0))
                        (lamC 'x
                              (set-boxC (idC 'global)
                                        (addC (numC 1)
                                              (unboxC (idC 'global))))))
                 (seqC (appC (idC 'counter) (numC 0))
                       (seqC (appC (idC 'counter) (numC 0))
                             (appC (idC 'counter) (numC 0)))))
          mt-env
          mt-mem)

  (test (interp*
         (withC 'newObj
                (withC 'classVar (boxC (numC 42))
                       (lamC 'x
                             (withC 'objField (boxC (idC 'x))
                                    (lamC 'y
                                          (withC 'old (unboxC (idC 'objField))
                                                 (seqC (set-boxC (idC 'objField)
                                                                 (idC 'y))
                                                       (idC 'old)))))))
                (withC 'objA (appC (idC 'newObj) (numC 0))
                       (withC 'objB (appC (idC 'newObj) (numC 1))
                              (seqC (appC (idC 'objA) (numC 5))
                                    (appC (idC 'objB) (numC 6)))))))
        #;(numV 0) ;; => change A then change B returns A's old value
        (numV 1) ;; => change A then change B returns B's old value
        ;; ==> means they don't share state (instance variable)
        #;(numV 5) ;; => change A then change B returns A's new value
        ;; ==> means they share state   (class/static variable)
        #;(numV 6) ;; => change A then change B returns B's new value
        )

  (test (interp* (withC 'x (numC 3)
                        (seqC (set-varC 'x (numC 4))
                              (idC 'x))))
        (numV 4))

  (test (interp* (withC 'f (lamC 'x
                                 (seqC (set-varC 'x (addC (idC 'x) (numC 1)))
                                       (idC 'x)))
                        (appC (idC 'f) (numC 3))))
        (numV 4))

  (test (interp* (withC 'x (numC 3)
                        (withC 'f (lamC 'x
                                        (seqC (set-varC 'x (addC (idC 'x) (numC 1)))
                                              (idC 'x)))
                               (addC (appC (idC 'f) (idC 'x))
                                     (idC 'x)))))
        (numV 7))

  (test (interp* (withC 'y (numC 3)
                        (withC 'f (lamC 'x
                                        (seqC (set-varC 'x (addC (idC 'x) (numC 1)))
                                              (idC 'x)))
                               (addC (appC (idC 'f) (idC 'y))
                                     (idC 'y)))))
        (numV 7))

  (test (interp* (withC 'y (boxC (numC 3))
                        (withC 'f (lamC 'x-ptr
                                        (set-boxC (idC 'x-ptr)
                                                  (addC (unboxC (idC 'x-ptr)) (numC 1))))
                               (addC (appC (idC 'f) (idC 'y))
                                     (unboxC (idC 'y))))))
        (numV 8))

  (test (interp*
         (withC 'x (numC 1)
                (withC 'y (numC 2)
                       (seqC (set-varC 'y
                                       (set-varC 'x (addC (idC 'x) (numC 1))))
                             (idC 'x)))))
        (numV 2))


  (define (fac n)
    (if (zero? n)
      1
      (* n (fac (- n 1)))))

  ;; int a = 1;
  ;; while ( n ) {
  ;;  a *= n--;
  ;; }
  
  (test (interp*
         (withC 'fac (numC 42)
                (seqC
                 (lamC 'x
                       (if0C (idC 'x)
                             (numC 1)
                             (mulC (idC 'x)
                                   (appC (idC 'fac)
                                         (subC (idC 'x) (numC 1))))))
                 (appC (lamC 'x
                             (if0C (idC 'x)
                                   (numC 1)
                                   (mulC (idC 'x)
                                         (appC (lamC 'x
                             (if0C (idC 'x)
                                   (numC 1)
                                   (mulC (idC 'x)
                                         (appC (lamC 'x
                             (if0C (idC 'x)
                                   (numC 1)
                                   (mulC (idC 'x)
                                         (appC (lamC 'x
                             (if0C (idC 'x)
                                   (numC 1)
                                   (mulC (idC 'x)
                                         (appC (idC 'fac)
                                               (subC (idC 'x) (numC 1))))))
                                               (subC (idC 'x) (numC 1))))))
                                               (subC (idC 'x) (numC 1))))))
                                               (subC (idC 'x) (numC 1))))))
                       (numC 3)))))
        (numV (fac 3)))
  
  (test (interp*
         (withC 'fac (numC 24)
                (seqC
                 (set-varC 'fac
                           (lamC 'x
                                 (if0C (idC 'x)
                                       (numC 1)
                                       (mulC (idC 'x)
                                             (appC (idC 'fac)
                                                   (subC (idC 'x) (numC 1)))))))
                 (appC (idC 'fac)
                       (numC 25)))))
        (numV (fac 25)))

  (define (recC [name : symbol] [named-expr : ExprC] [body : ExprC]) : ExprC
    (withC name (numC 42) ;; JS has "undefined" rather than 42
           (seqC (set-varC name named-expr)
                 body)))

  (test (interp* (recC 'wreckC (addC (idC 'wreckC) (numC 1))
                       (idC 'wreckC)))
        ;; should be
        #;(numV +inf.0)
        ;; actually
        (numV 43))
  
  (test (interp*
         (recC 'fac (lamC 'x
                          (if0C (idC 'x)
                                (numC 1)
                                (mulC (idC 'x)
                                      (appC (idC 'fac)
                                            (subC (idC 'x) (numC 1))))))
               (appC (idC 'fac)
                     (numC 25))))
        (numV (fac 25)))

  (test (interp*
         (withC 'make-fac
                (lamC 'make-fac
                      ;; This is actually factorial (assuming that make-fac gives us factorial)
                      (withC 'fac
                             (lamC 'x
                                   (appC (appC (idC 'make-fac) (idC 'make-fac))
                                         (idC 'x)))
                             (lamC 'x
                                   (if0C (idC 'x)
                                         (numC 1)
                                         (mulC (idC 'x)
                                               (appC (idC 'fac)
                                                     (subC (idC 'x) (numC 1))))))))
                (withC 'fac
                       (appC (idC 'make-fac)
                             (idC 'make-fac))                    
                       (appC (idC 'fac)
                             (numC 25)))))
        (numV (fac 25)))

  (define (beautiful-recC [name : symbol] [named-expr : ExprC] [body : ExprC]) : ExprC
    (withC 'make-name
           (lamC 'SOMETHING
                 (withC name
                        (lamC 'x
                              (appC (appC (idC 'SOMETHING)
                                          (idC 'SOMETHING))
                                    (idC 'x)))
                        named-expr))
           (withC name
                  (appC (idC 'make-name)
                        (idC 'SOMETHING))
                  body)))

  ;;
  ;; FIX POINT of F is a value X, s.t. F(X) = X
  ;;
  ;; g(x) = 3 * x, fix points: 0
  ;; h(x) = x    , everything is a fix point
  ;; z(x) = x + 2, fix points: .... none ....
  ;;
  ;; make-name(make-name) = name
  ;; name = make-name(make-name)
  ;;
  ;; name = make-name(name)
  ;;
  ;; make-name finds the FIX POINT of the factorial function
  ;; make-name takes (n copy & pastes) and returns (n+1 copy & pastes)
  ;;
  ;; name = infinity copy & pastes
  ;;
  ;; the only number, X, where X = X + 1 is "infinity"
  ;; Make-Fac(x) = x + 1 factorial, fix points: infinite factorial
  ;;

  (test (interp*
         (beautiful-recC
          'fac (lamC 'x
                     (if0C (idC 'x)
                           (numC 1)
                           (mulC (idC 'x)
                                 (appC (idC 'fac)
                                       (subC (idC 'x) (numC 1))))))
          (appC (idC 'fac)
                (numC 25))))
        (numV (fac 25)))


  ;;
  ;; Optimization
  ;;

  ;; Programmer wrote P          (x * 2) (x + x) (x * (1 + 1)) (x * (4 - 3))
  ;; FIGURE OUT: P'
  ;; KNOW: P' and P do the same thing
  ;; KNOW: P' is better than P         (x << 1)
  ;; Compile outputs P'          (x << 1)

  (define (weird-even? x)
    (if (zero? x)
      #t
      (weird-odd? (- x 1))))
  (define (weird-odd? x)
    (if (zero? x)
      #f
      (weird-even? (- x 1))))
  
  )
