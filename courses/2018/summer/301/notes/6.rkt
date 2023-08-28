#lang racket/base
(require racket/match
         racket/list
         racket/pretty)
(module+ test
  (require chk))

;; ISWIM
(struct expr () #:transparent)
(struct :var expr (x) #:transparent)
(struct :lam expr (x e) #:transparent)
(struct :app expr (rator rand) #:transparent)
(struct :con expr (b) #:transparent)
(struct :prim expr (o args) #:transparent)

(module+ test
  (define id (:lam 'x (:var 'x)))
  (define ex1 (:app id (:con 5)))
  (define (pow2 n)
    (cond
      [(zero? n) (:con 1)]
      [else
       (:app (:lam 'nm1 (:prim + (list (:var 'nm1) (:var 'nm1))))
             (pow2 (sub1 n)))]))
  (pretty-print (pow2 5)))

(define (V? e)
  (or (:con? e) (:lam? e)))

(struct ctxt () #:transparent)
(struct E:hole ctxt () #:transparent)
(struct E:app-left ctxt (E n) #:transparent)
(struct E:app-right ctxt (v E) #:transparent)
(struct E:prim ctxt (o vs E ms) #:transparent)

(define (plug E w)
  (match E
    [(E:hole) w]
    [(E:app-left E n)
     (:app (plug E w) n)]
    [(E:app-right v E)
     (:app v (plug E w))]
    [(E:prim o vs E ms)
     (:prim o (append vs (cons (plug E w) ms)))]))

(define (find-context e)
  (match e
    [(:var _) #f]
    [(:lam _ _) (cons (E:hole) e)]
    [(:con _) #f]
    [(:app m n)
     (cond [(not (V? m))
            (match (find-context m)
              [#f #f]
              [(cons E w)
               (cons (E:app-left E n) w)])]
           [(not (V? n))
            (match (find-context n)
              [#f #f]
              [(cons E w)
               (cons (E:app-right m E) w)])]
           [else
            (cons (E:hole) e)])]
    [(:prim o (list m1 m2))
     (cond [(not (V? m1))
            (match (find-context m1)
              [#f #f]
              [(cons E w)
               (cons (E:prim o empty E (list m2)) w)])]
           [(not (V? m2))
            (match (find-context m2)
              [#f #f]
              [(cons E w)
               (cons (E:prim o (list m1) E empty) w)])]
           [else
            (cons (E:hole) e)])]))

(define (reduce w)
  (match w
    [(:app (:lam x m) v)
     (subst x v m)]
    [(:prim o (list (:con n1) (:con n2)))
     (:con (o n1 n2))]))

(define (subst x v m)
  (match m
    [(:var y)
     (if (eq? x y) v m)]
    [(:app m n)
     (:app (subst x v m) (subst x v n))]
    [(:prim o ms)
     (:prim o (for/list ([m (in-list ms)])
                (subst x v m)))]
    [(:lam y b)
     (if (eq? x y) m
         (error 'XXX))]))

(define (interp e)
  (match (find-context e)
    [#f #f]
    [(cons E w)
     (plug E (reduce w))]))

(define (interp* e)
  (pretty-print e)
  (define ep (interp e))
  (if ep (interp* ep) e))

(module+ test
  (chk (interp* ex1)
       (:con 5))
  (chk (interp* (pow2 5))
       (:con (expt 2 5))))
