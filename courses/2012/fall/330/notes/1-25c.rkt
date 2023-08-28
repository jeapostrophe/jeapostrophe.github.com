#lang plai/collector
(define heap-ptr 'uninitialized-heap-ptr)

(define (init-allocator)
  (printf "(init-allocator)\n")
  (set! heap-ptr 0))

(define (gc:alloc-flat p)
  (printf "(gc:alloc-flat ~a)\n" p)
  (begin
    (when (> (+ heap-ptr 2) (heap-size))
      (error 'gc:alloc-flat "out of memory"))
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ 1 heap-ptr) p)
    (set! heap-ptr (+ 2 heap-ptr))
    
    (- heap-ptr 2)))

(define (gc:cons f r)
  (printf "(gc:cons ~a ~a)\n" f r)
  (begin
    (when (> (+ heap-ptr 3) (heap-size))
      (error 'gc:cons "out of memory"))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) f)
    (heap-set! (+ 2 heap-ptr) r)
    (set! heap-ptr (+ 3 heap-ptr))
    (- heap-ptr 3)))

(define (gc:cons? a)
  (printf "(gc:cons? ~a)\n" a)
  (eq? (heap-ref a) 'cons))

(define (gc:first a)
  (printf "(gc:first ~a)\n" a)
  (heap-ref (+ 1 a)))

(define (gc:rest a)
  (printf "(gc:rest ~a)\n" a)
  (heap-ref (+ 2 a)))

(define (gc:set-first! a f)
  (printf "(gc:set-first! ~a ~a)\n" a f)
  (if (gc:cons? a)
      (heap-set! (+ 1 a) f)
      (error 'set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (printf "(gc:set-rest! ~a ~a)\n" a r)
  (heap-set! (+ 2 a) r))

(define (gc:flat? a)
  (printf "(gc:flat? ~a)\n" a)
  (eq? (heap-ref a) 'prim))

(define (gc:deref a)
  (printf "(gc:deref ~a)\n" a)
  (heap-ref (+ 1 a)))