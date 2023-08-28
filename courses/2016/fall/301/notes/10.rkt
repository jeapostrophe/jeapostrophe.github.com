#lang plai-typed

;; Modifying data structures
;;  Posn p = new Posn (3.0, 4.5);
;;  f(p) => Ya!
;;  p.x = 5.0;
;;  f(p) => Nah!

;; identifier "identifies a value"
;; (define x (+ 4 5))

;; variable "is a location"

;; Modifying variables
;;  int f(int x) {
;;   x = x + 1;
;;   return x;
;; }
;; f(5) => 6

;; Modifying is technically called "mutation"

;; 1. What can we now, that we couldn't do without mutation?
;; ===> Non-mutation (purity) simulates mutation
;; 2. Mutation is more "convenient"/"usability"/"I like it"
;; ===> embedding a simulation is painful
;; 3. Structure abstraction
;; ===> c.f. iteration
;; ===> next() = value or NULL
;;      next : -> Maybe value
;; pure-next : Memory -> Memory' x Maybe value

;; next() ----> A

;; next() \_____ B
;; next() /

;; + != A + B

;; Mutation adds TIME to the analysis

;; Purity FORCES TIME into the PROGRAM when you want MUTATION

(module+ main
  (define-type ExprC
    [numC (n : number)]
    [addC (l : ExprC) (r : ExprC)]
    [mulC (l : ExprC) (r : ExprC)]
    [idC (s : symbol)]
    ;; pass-by-value
    [lamC (arg : symbol) (body : ExprC)]
    ;; pass-by-reference
    [&lamC (arg : symbol) (body : ExprC)]
    [appC (fun : ExprC) (arg : ExprC)]
    [boxC (v : ExprC)]
    [unboxC (b : ExprC)]
    [set-boxC (b : ExprC) (v : ExprC)]
    [seqC (fst : ExprC) (snd : ExprC)]
    ;; x = 4
    #;(set-varC 'x (numC 4))

    ;; Why not?
    #;(set-varC (idC 'x) (numC 4))
    [set-varC (s : symbol) (val : ExprC)])

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
    ;; We don't do this:
    #;[pv&m (ptr : Pointer) (mem : Memory)]
    ;; val ==== "RAX"
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

     ;; Why don't we write this
     ;; set-varC : expr expr -> expr
     #;(set-varC (idC 'x) (numC 4))
     ;; Rather than
     ;; set-varC : sym expr -> expr
     #;(set-varC 'x (numC 4))

     ;; Is this okay?
     ;; y = x + 3
     ;; x + 3 = 4
     ;; z = y + x + 3
     ;; =====>
     ;; RAX = memory_lookup(&x) + 3
     ;; memory_store(&y, RAX)
     ;; RAX = RAX << 1
     ;; memory_store(&z, RAX)

     ;; C: x + 3 = 4 ===> "x + 3 is not an l-value"
     #;(set-varC (addC (idC 'x) (numC 3)) (numC 4))

     ;; C: *(x + 3) = 4 ===> "x is a pointer, add three to that pointer, modify that memory"

     ;; struct { int q; int p; } x = ....
     ;; C: x->p = 4
     ;; C: *((int)(((char)x) + sizeof(int))) = 4

     ;; C: f(x) = 4
     #;(set-varC (appC (idC 'f) (idC 'x)) (numC 4))

     ;; What is an l-value?
     ;; ===> something with an address
     ;; What is a temporary?
     ;; ===> something in a register

     [set-varC
      (s v)
      ;; env : sym -> ptr
      ;; mem : ptr -> val
      (local [(define ptr-to-s (env-lookup s env))
              (define new-val&mem (interp v env mem))
              (define new-val (v&m-val new-val&mem))]
        (v&m new-val
             (override-mem (cell ptr-to-s new-val)
                           #;mem ;; The memory that we started with
                           (v&m-mem new-val&mem) ;; The memory after evaluating v
                           )))]

     [lamC
      (arg body)
      (v&m (cloV expr env)
           mem)]
     [&lamC
      (arg body)
      (v&m (cloV expr env)
           mem)]
     [appC
      (f a)
      (local [(define f-v&m (interp f env mem))
              (define fd (v&m-val f-v&m))
              (define a-v&m (interp a env (v&m-mem f-v&m)))
              (define new-ptr (malloc (v&m-mem a-v&m)))]
        (type-case
         ExprC (cloV-lam fd)
         [lamC
          (arg body)
          (interp body
                  (extend-env (bind arg
                                    new-ptr)
                              (cloV-env fd))
                  (override-mem (cell new-ptr (v&m-val a-v&m))
                                (v&m-mem a-v&m)))]
         [&lamC
          (arg body)
          (local [(define args-actual-ptr (env-lookup (idC-s a) env))]
            (interp body
                    (extend-env (bind arg
                                      args-actual-ptr)
                                (cloV-env fd))
                    ;; Memory stays the same (no new values)
                    (v&m-mem a-v&m)))]
         [else
          (error 'interp "Not given a lambda in a closure")]))]

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


     ;; f : int -> *int
     ;; *(f(x)) = 5
     ;; --> set-box

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
        (interp snd env (v&m-mem f-v&m)))]))

  (define (interp* [expr : ExprC]) : Val
    (v&m-val (interp expr mt-env mt-mem)))

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

  ;; int x = 3;
  ;; x = 4;
  ;; return x;
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
        ;; If this returns 8, then 'x refers to the same memory
        (numV 7))

  ;; int f(int x) { x = x + 1 ; return x }
  ;; int main() { int y = 3; return f(y) + y; }
  ;; ==> 7
  (test (interp* (withC 'y (numC 3)
                        (withC 'f (lamC 'x
                                        (seqC (set-varC 'x (addC (idC 'x) (numC 1)))
                                              (idC 'x)))
                               (addC (appC (idC 'f) (idC 'y))
                                     (idC 'y)))))
        ;; If this returns 8, then
        (numV 7))

  ;; int f(int *x) { *x = *x + 1 ; return *x }
  ;; int main() { int y = 3; return f(&y) + y; }
  ;; ==> 8
  (test (interp* (withC 'y (boxC (numC 3))
                        (withC 'f (lamC 'x-ptr
                                        (set-boxC (idC 'x-ptr)
                                                  (addC (unboxC (idC 'x-ptr)) (numC 1))))
                               (addC (appC (idC 'f) (idC 'y))
                                     (unboxC (idC 'y))))))
        (numV 8))

  ;; & means "pass by reference" (C++)
  ;; int f(int &x) { x = x + 1 ; return x }
  ;; int main() { int y = 3; return f(y) + y; }
  ;; ==> 8
  (test (interp* (withC 'y (numC 3)
                        (withC 'f (&lamC 'x
                                         (seqC (set-varC 'x (addC (idC 'x) (numC 1)))
                                               (idC 'x)))
                               (addC (appC (idC 'f) (idC 'y))
                                     (idC 'y)))))
        (numV 8))

  ;; int x = 1
  ;; int y = 2
  ;; y = (x = x + 1)
  ;; return x;
  (test (interp*
         (withC 'x (numC 1)
                (withC 'y (numC 2)
                       (seqC (set-varC 'y
                                       (set-varC 'x (addC (idC 'x) (numC 1))))
                             (idC 'x)))))
        (numV 2))

  )
