#lang plai-typed

(module+ day8
  (define-type ExprC
    [numC (n : number)]
    [addC (l : ExprC) (r : ExprC)]
    [mulC (l : ExprC) (r : ExprC)]
    [idC (s : symbol)]
    [lamC (arg : symbol) (body : ExprC)]
    [appC (fun : ExprC) (arg : ExprC)])

  (define (subC [l : ExprC] [r : ExprC]) : ExprC
    (addC l (mulC (numC -1) r)))

  (define (negC [o : ExprC]) : ExprC
    (subC (numC 0) o))

  (define (withC [name : symbol] [named-expr : ExprC] [body : ExprC]) : ExprC
    (appC (lamC name body) named-expr))

  (define-type Val
    [numV (n : number)]
    [cloV (lam : ExprC) (env : Env)])

  (define-type Binding
    [bind (name : symbol) (val : Val)])

  (define-type-alias Env (listof Binding))
  (define mt-env empty)
  (define extend-env cons)

  (define (lookup [for : symbol] [env : Env]) : Val
    (cond
      [(empty? env) (error 'lookup "name not found")]
      [(symbol=? for (bind-name (first env)))
       (bind-val (first env))]
      [else (lookup for (rest env))]))

  (define (numV-binop [o : (number number -> number)] [l : Val] [r : Val]) : Val
    (numV (o (numV-n l) (numV-n r))))

  (define (interp [expr : ExprC] [env : Env]) : Val
    (type-case
     ExprC expr
     [numC (n) (numV n)]
     [addC (l r) (numV-binop + (interp l env) (interp r env))]
     [mulC (l r) (numV-binop * (interp l env) (interp r env))]

     [idC (n) (lookup n env)]
     [lamC (arg body) (cloV expr env)]
     [appC
      (f a)
      (local [(define fd (interp f env))]
        (interp (lamC-body (cloV-lam fd))
                (extend-env (bind (lamC-arg (cloV-lam fd))
                                  (interp a env))
                            (cloV-env fd))))]))

  (define (interp* [expr : ExprC]) : Val
    (interp expr mt-env))

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
  (test (interp* (withC 'x (numC 6)
                        (idC 'x)))
        (numV 6))
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

  )


;; int x = 5;
;; printf("%d\n", x);
;; x = 6;
;; printf("%d\n", x);
;;
;; ==>
;; 5
;; 6

;; x = new_int(5);
;; printf("%d\n", lookup_int(x));
;; change_int(x, 6);
;; printf("%d\n", lookup_int(x));

(module+ main
  (define-type ExprC
    [numC (n : number)]
    [addC (l : ExprC) (r : ExprC)]
    [mulC (l : ExprC) (r : ExprC)]
    [idC (s : symbol)]
    [lamC (arg : symbol) (body : ExprC)]
    [appC (fun : ExprC) (arg : ExprC)]

    ;; This is like mallocing a new pointer to a value
    [newC (init : ExprC)]
    ;; This is like a pointer deref
    [lookupC (container : ExprC)]
    ;; This is like modifying the pointer
    [changeC (container : ExprC) (new-val : ExprC)]
    ;; This is like the ";" or "," operators in C and the "begin" form
    ;; in Racket
    [seqC (fst : ExprC) (snd : ExprC)])

  (define (subC [l : ExprC] [r : ExprC]) : ExprC
    (addC l (mulC (numC -1) r)))

  (define (negC [o : ExprC]) : ExprC
    (subC (numC 0) o))

  (define (withC [name : symbol] [named-expr : ExprC] [body : ExprC]) : ExprC
    (appC (lamC name body) named-expr))

  (define-type Val
    [numV (n : number)]
    [cloV (lam : ExprC) (env : Env)]
    ;; See how a box is just a pointer (which is just a special number)
    [boxV (ptr : Pointer)])

  ;; Environments map names to pointers (memory locations); C works
  ;; the same way
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

  ;; Memory maps pointers to values; C works the same way, but with a
  ;; weaker notion of value
  (define-type-alias Pointer number)
  (define-type Cell
    [cell (ptr : Pointer) (val : Val)])
  (define-type-alias Memory (listof Cell))
  (define mt-mem empty)
  (define extend-mem cons)

  ;; Really malloc just needs to "find a pointer that is not
  ;; used". This is guaranteed to do that, but may skip values
  (define (malloc [mem : Memory]) : Pointer
    (length mem))

  (define (mem-lookup [for : Pointer] [env : Memory]) : Val
    (cond
      [(empty? env) (error 'mem-lookup "Segmentation Fault")]
      [(= for (cell-ptr (first env)))
       (cell-val (first env))]
      [else (mem-lookup for (rest env))]))

  (define (numV-binop [o : (number number -> number)] [l : Val] [r : Val]) : Val
    (numV (o (numV-n l) (numV-n r))))

  ;; Our interpreter now returns the value plus a new memory
  (define-type Val&&Memory
    [v&m (val : Val) (mem : Memory)])

  (define (interp [expr : ExprC] [env : Env] [mem : Memory]) : Val&&Memory
    (type-case
     ExprC expr
     [numC (n) (v&m (numV n) mem)]
     [addC
      (l r)
      ;; i = 4
      ;; (i++)+i
      ;; => 9
      (local [(define l-v&m (interp l env mem))
              ;; using (v&m-mem l-v&m) means the LEFT happens first
              ;; then the RIGHT
              ;;
              ;; what if we used "mem"?
              (define r-v&m (interp r env (v&m-mem l-v&m)))]
        (v&m (numV-binop + (v&m-val l-v&m) (v&m-val r-v&m))
             (v&m-mem r-v&m)))]
     [mulC
      (l r)
      ;; mem goes in ==>
      ;; ;; mem goes in and (v&m-mem l-v&m) comes out
      ;; ;; (v&m-mem l-v&m) goes in and (v&m-mem r-v&m) comes out
      ;; ==> (v&m-mem r-v&m) comes out
      ;;
      ;; This is called LINEARITY
      (local [(define l-v&m (interp l env mem))
              (define r-v&m (interp r env (v&m-mem l-v&m)))]
        (v&m (numV-binop * (v&m-val l-v&m) (v&m-val r-v&m))
             (v&m-mem r-v&m)))]

     [idC
      (n)
      ;; name -> ptr -> value
      (v&m (mem-lookup (env-lookup n env) mem)
           mem)]
     [lamC
      (arg body)
      (v&m (cloV expr env)
           ;; No change to memory
           mem)]
     [appC
      (f a)
      (local [(define f-v&m (interp f env mem))
              (define fd (v&m-val f-v&m))
              (define a-v&m (interp a env (v&m-mem f-v&m)))
              ;; Alloc a place for the argument to go
              (define new-ptr (malloc (v&m-mem a-v&m)))]
        (interp (lamC-body (cloV-lam fd))
                ;; Extends the lexical namespace
                (extend-env (bind (lamC-arg (cloV-lam fd))
                                  new-ptr)
                            (cloV-env fd))
                ;; Extends the memory as well
                (extend-mem (cell new-ptr (v&m-val a-v&m))
                            (v&m-mem a-v&m))))]

     [newC
      (iv)
      (local [(define iv-v&m (interp iv env mem))
              (define new-ptr (malloc (v&m-mem iv-v&m)))
              (define new-mem
                (extend-mem (cell new-ptr
                                  (v&m-val iv-v&m))
                            ;; mem ;; => means ignore the effects inside of iv
                            (v&m-mem iv-v&m)))]
        (v&m (boxV new-ptr)
             new-mem))]
     [lookupC
      (c)
      (local [(define c-v&m (interp c env mem))
              (define c-ptr (boxV-ptr (v&m-val c-v&m)))]
        (v&m (mem-lookup c-ptr (v&m-mem c-v&m))
             (v&m-mem c-v&m)))]
     [changeC (c nv)
              (local
                  [;; c is an expr for a box
                   (define b-v&m (interp c env mem)) ;; => the actual box
                   (define box-ptr (boxV-ptr (v&m-val b-v&m)))
                   ;; nv is expr for the new value
                   (define nv-v&m (interp nv env (v&m-mem b-v&m)))]
                ;; Put the nv inside the box
                (v&m (v&m-val nv-v&m)
                     (extend-mem (cell box-ptr (v&m-val nv-v&m))
                                 (v&m-mem nv-v&m))))]
     [seqC (fst snd)
           (local [(define f-v&m (interp fst env mem))]
             ;; Notice that we ignore (v&m-val f-v&m) because we only look at fst for its effects
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

  ;; int x = 5;
  ;; print x;
  ;; x = 6;
  ;; print x;
  (test (interp*
         (withC 'x (newC (numC 5))
                (addC (lookupC (idC 'x))
                      (seqC
                       (changeC (idC 'x)
                                (numC 6))
                       (lookupC (idC 'x))))))
        (numV 11))

  (test (interp* (withC 'x (numC 5)
                        (addC (idC 'x)
                              (addC (withC 'x (numC 6)
                                           (idC 'x))
                                    (idC 'x)))))
        (numV 16))

  (test (interp* (withC 'global (newC (numC 0))
                        (withC 'counter
                               (lamC 'x
                                     (changeC (idC 'global)
                                              (addC (numC 1)
                                                    (lookupC (idC 'global)))))
                               (seqC (appC (idC 'counter) (numC 0))
                                     (seqC (appC (idC 'counter) (numC 0))
                                           (appC (idC 'counter) (numC 0)))))))
        (numV 3))

  (test (interp* (withC 'counter
                        (lamC 'x
                              (withC 'global (newC (numC 0))
                                     (changeC (idC 'global)
                                              (addC (numC 1)
                                                    (lookupC (idC 'global))))))
                        (seqC (appC (idC 'counter) (numC 0))
                              (seqC (appC (idC 'counter) (numC 0))
                                    (appC (idC 'counter) (numC 0))))))
        (numV 1))

  (test (interp* (withC 'counter
                        (withC 'global (newC (numC 0))
                               (lamC 'x
                                     (changeC (idC 'global)
                                              (addC (numC 1)
                                                    (lookupC (idC 'global))))))
                        (seqC (appC (idC 'counter) (numC 0))
                              (seqC (appC (idC 'counter) (numC 0))
                                    (appC (idC 'counter) (numC 0))))))
        (numV 3))

  (interp (withC 'counter
                 (withC 'global (newC (numC 0))
                        (lamC 'x
                              (changeC (idC 'global)
                                       (addC (numC 1)
                                             (lookupC (idC 'global))))))
                 (seqC (appC (idC 'counter) (numC 0))
                       (seqC (appC (idC 'counter) (numC 0))
                             (appC (idC 'counter) (numC 0)))))
          mt-env
          mt-mem)

  #;(v&m (numV 3)
       (list (cell 0 (numV 3))
             (cell 7 (numV 0))
             (cell 0 (numV 2))
             (cell 5 (numV 0))
             (cell 0 (numV 1))
             (cell 3 (numV 0))
             (cell 2
                   (cloV (lamC 'x
                               (changeC (idC 'global)
                                        (addC (numC 1)
                                              (lookupC (idC 'global)))))
                         (list
                          (bind 'global 1))))
             (cell 1 (boxV 0))
             (cell 0 (numV 0))))

  (test (interp*
         (withC 'newObj
                (withC 'classVar (newC (numC 42))
                       (lamC 'x
                             (withC 'objField (newC (idC 'x))
                                    (lamC 'y
                                          (withC 'old (lookupC (idC 'objField))
                                                 (seqC (changeC (idC 'objField)
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
  

  ;; Names (i.e. the env) is LEXICAL
  ;; Values (i.e. the memory) is DYNAMIC/GLOBAL
  
  )
