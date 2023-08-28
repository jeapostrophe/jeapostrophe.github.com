#lang racket/base
(require racket/match)

(struct :num (n))
(struct :bool (b))
(struct :prim (p))
(struct :if (c t f))
(struct :app (f args))

(define (interp e)
  (match e
    [(:num n) n]
    [(:bool b) b]
    [(:prim p) p]
    [(:if c t f) (interp (if (interp c) t f))]
    [(:app f args) (apply (interp f) (map interp args))]))

(define PRIM->FUN
  (hasheq '+ +
          '= =))

(define (desugar se)
  (match se
    [(? number? n) (:num n)]
    [(? boolean? b) (:bool b)]
    [(? symbol? p) (:prim (hash-ref PRIM->FUN p))]
    [(list 'if c t f) (:if (desugar c) (desugar t) (desugar f))]
    [(cons f args) (:app (desugar f) (map desugar args))]))

(module+ test
  (interp
   (:if (:app (:prim =) (list (:num 4) (:num 2)))
        (:num 5)
        (:num 6)))
  (interp
   (desugar
    '(if (= 4 2) 5 6))))
