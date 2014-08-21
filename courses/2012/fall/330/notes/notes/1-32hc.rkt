#lang plai/collector

;; Trivial allocator
;; (define heap-ptr 'uninitialized-heap-ptr)

;; (define (init-allocator)
;;   (set! heap-ptr 0))

;; (define (alloc)
;;   (when (> (+ heap-ptr 2) (heap-size))
;;     (error 'alloc "out of memory"))
;;   (set! heap-ptr (+ heap-ptr 2))
;;   (- heap-ptr 2))

;; (define (gc:alloc-flat p)
;;   (define p-addr (alloc))
;;   (heap-set! (+ 0 p-addr) 'prim)
;;   (heap-set! (+ 1 p-addr) p)
;;   p-addr)

;; (define (gc:cons f r)
;;   (define c-addr (alloc))
;;   (heap-set! (+ 0 c-addr) f)
;;   (heap-set! (+ 1 c-addr) r)
;;   c-addr)

;; (define (gc:cons? a)
;;   (not (gc:flat? a)))

;; (define (gc:first a)
;;   (if (gc:cons? a)
;;     (heap-ref (+ 0 a))
;;     (error 'first "expects address of cons")))

;; (define (gc:rest a)
;;   (if (gc:cons? a)
;;     (heap-ref (+ 1 a))
;;     (error 'rest "expects address of cons")))

;; (define (gc:set-first! a f)
;;   (if (gc:cons? a)
;;     (heap-set! (+ 0 a) f)
;;     (error 'set-first! "expects address of cons")))

;; (define (gc:set-rest! a r)
;;   (if (gc:cons? a)
;;     (heap-set! (+ 1 a) r)
;;     (error 'set-rest! "expects address of cons")))

;; (define (gc:flat? a)
;;   (eq? (heap-ref a) 'prim))

;; (define (gc:deref a)
;;   (if (gc:flat? a)
;;     (heap-ref (+ 1 a))
;;     (error 'deref "expects address of prim")))

;; Mark & Sweep
(define free-list-head-ptr 'uninitialized-heap-ptr)

(define (init-allocator)
  (for ([a (in-range (heap-size))])
    (heap-set! a #f))
  (sweep))

(define (deodd x)
  (if (even? x)
    x
    (sub1 x)))

(define (odd x)
  (if (even? x)
    (add1 x)
    x))

(define (usable-heap-size)
  (deodd (heap-size)))

(define (last-real-addr some-size)
  (- some-size 2))

(test (last-real-addr 4) 2)
(test (last-real-addr 6) 4)

(define (sweep)
  (define (sweep-from some-addr)
    (cond
      [(some-addr . >= . (usable-heap-size))
       #f]
      [(marked? some-addr)
       (unmark some-addr)
       (sweep-from (+ 2 some-addr))]
      [else
       (heap-set! (+ 0 some-addr) 'free)
       (heap-set! (+ 1 some-addr)
                  (sweep-from (+ 2 some-addr)))      
       some-addr]))
  (set! free-list-head-ptr (sweep-from 0)))

(define (collect get-root-set)
  (mark-roots (get-root-set))
  (sweep))

(define (mark-roots rs)
  (for-each mark-root rs))
(define (mark-root r)
  (mark-obj (read-root r)))
(define (mark-obj addr)
  (cond
    [(marked? addr)
     (void)]
    [(gc:cons? addr)
     (define f (gc:first addr))
     (mark-cons addr)
     (mark-obj f)
     (mark-obj (gc:rest addr))]
    [(gc:flat? addr)
     (define v (gc:deref addr))
     (mark-flat addr)
     (mark-roots (maybe-procedure-roots v))]))

(define (maybe-procedure-roots v)
  (if (procedure? v)
    (procedure-roots v)
    empty))

(define (alloc get-root-set [back-again? #f])

  (cond
    [free-list-head-ptr
     (define new-addr free-list-head-ptr)
     (set! free-list-head-ptr
           (heap-ref (+ 1 new-addr)))
     new-addr]
    [(not back-again?)
     (collect get-root-set)
     (alloc get-root-set #t)]
    [else
     (error 'alloc "out of memory")]))

(define (gc:alloc-flat p)
  (define p-addr (alloc (λ () (append (get-root-set) (maybe-procedure-roots p)))))
  (heap-set! (+ 0 p-addr) 'prim)
  (heap-set! (+ 1 p-addr) p)
  p-addr)

(define (gc:cons f r)
  (define c-addr (alloc (λ () (get-root-set f r))))
  (heap-set! (+ 0 c-addr) f)
  (heap-set! (+ 1 c-addr) r)
  c-addr)

(define (gc:cons? a)
  (not (gc:flat? a)))

(define (mark-cons a)
  (heap-set! a (odd (heap-ref a))))

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

(define (mark-flat a)
  (heap-set! a 'marked-prim))

(define (gc:deref a)
  (if (gc:flat? a)
    (heap-ref (+ 1 a))
    (error 'deref "expects address of prim")))

(define (marked? a)
  (or (eq? (heap-ref a) 'marked-prim)
      (and (number? (heap-ref a))
           (odd? (heap-ref a)))))

(define (unmark a)
  (define v (heap-ref a))
  (cond
    [(eq? v 'marked-prim)
     (heap-set! a 'prim)]
    [else
     (heap-set! a (deodd v))]))
