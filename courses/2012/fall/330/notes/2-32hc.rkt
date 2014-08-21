#lang plai/collector

;; Trivial allocator
(define free-ptr #f)

(define (init-allocator)
  (sweep))

(define (usable-heap-size)
  (deodd (heap-size)))

(define (deodd v)
  (if (odd? v)
    (sub1 v)
    v))
(define (odd v)
  (if (odd? v)
    v
    (add1 v)))

(define (sweep)
  (define (sweep/from a)
    (cond
      [(a . >= . (usable-heap-size))
       #f]
      [(marked? a)
       (unmark a)
       (sweep/from (+ 2 a))]
      [else
       (heap-set! (+ 0 a) 'free)
       (heap-set! (+ 1 a) 
                  (sweep/from (+ 2 a)))       
       a]))
  (set! free-ptr
        (sweep/from 0)))

(define (collect get-root-set)
  (mark-roots (get-root-set))
  (sweep))

(define (mark-roots rs)
  (for-each mark-root rs))
(define (mark-root r)
  (mark-obj (read-root r)))

(define (mark-obj a)
  (cond
    [(marked? a)
     (void)]
    [(gc:cons? a)
     (define f (gc:first a))
     (mark-cons a)
     (mark-obj f)
     (mark-obj (gc:rest a))]
    [(gc:flat? a)
     (define v (gc:deref a))
     (mark-flat a)
     (mark-roots (maybe-procedure-roots v))]))

(define (maybe-procedure-roots v)
  (if (procedure? v)
    (procedure-roots v)
    empty))

(define (alloc get-root-set [try-again? #t])
  (cond
    [free-ptr
     (begin0 free-ptr
             (set! free-ptr (heap-ref (+ 1 free-ptr))))]
    [try-again?
     (collect get-root-set)
     (alloc get-root-set #f)]
    [else
     (error 'alloc "out of memory")]))

(define (gc:alloc-flat p)
  (define p-addr (alloc (λ () (append (maybe-procedure-roots p)
                                      (get-root-set)))))
  (heap-set! (+ 0 p-addr) 'prim)
  (heap-set! (+ 1 p-addr)     p)
  p-addr)

(define (gc:cons f r)
  (define c-addr (alloc (λ () (get-root-set f r))))
  (heap-set! (+ 0 c-addr)     f)
  (heap-set! (+ 1 c-addr)     r)
  c-addr)

(define (marked? a)
  (or (marked-flat? a)
      (marked-cons? a)))
(define (unmark a)
  (cond [(marked-flat? a)
         (heap-set! (+ 0 a) 'prim)]
        [(marked-cons? a)
         (heap-set! (+ 0 a) (deodd (heap-ref (+ 0 a))))]))
(define (marked-cons? a)
  (define v (heap-ref (+ 0 a)))
  (and (number? v)
       (odd? v)))
(define (mark-cons a)
  (heap-set! (+ 0 a) (odd (heap-ref (+ 0 a)))))
(define (marked-flat? a)
  (eq? (heap-ref (+ 0 a)) 'marked-prim))
(define (mark-flat a)
  (heap-set! (+ 0 a) 'marked-prim))

(define (gc:cons? a)
  (not (gc:flat? a)))

(define (gc:first a)
  (if (gc:cons? a)
    (heap-ref (+ 0 a))
    (error 'first "expects address of cons")))

(define (gc:rest a)
  (if (gc:cons? a)
    (heap-ref (+ 1 a))
    (error 'rest "expects address of cons")))

(define (gc:set-first! a f)
  (if (gc:cons? a)
    (heap-set! (+ 0 a) f)
    (error 'set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (if (gc:cons? a)
    (heap-set! (+ 1 a) r)
    (error 'set-rest! "expects address of cons")))

(define (gc:flat? a)
  (eq? (heap-ref a) 'prim))

(define (gc:deref a)
  (if (gc:flat? a)
    (heap-ref (+ 1 a))
    (error 'deref "expects address of prim")))
