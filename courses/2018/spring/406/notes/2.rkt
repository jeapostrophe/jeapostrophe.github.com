#lang racket/base
(require racket/match)
(module+ test
  (require chk))

;; e = num | (- e) | (+ e e) | var | (let ([var e]) e)
(struct e () #:transparent)
(struct :num e (n) #:transparent)
(struct :- e (e) #:transparent)
(struct :+ e (l r) #:transparent)
(struct :var e (x) #:transparent)
(struct :let e (x xe be) #:transparent)

;; p = (program e)
(struct :program (e) #:transparent)

;; parse : string -> p
(module+ test
  (define ex1
    (:program (:let 'x (:num 42)
                    (:+ (:var 'x) (:- (:var 'x))))))
  )

;; interp/env : env[var,ans] e -> ans
(define (interp/env env e)
  (match e
    [(:num n) n]
    [(:- e) (* -1 (interp/env env e))]
    [(:+ l r) (+ (interp/env env l)
                 (interp/env env r))]
    [(:var x) (hash-ref env x)]
    [(:let x xe be)
     (define new-env
       (hash-set env x (interp/env env xe)))
     (interp/env new-env be)]))

;; interp : p -> ans
(define (interp p)
  (match-define (:program eeeh) p)
  (interp/env (hasheq) eeeh))

(define (compile-p p)
  ;; XXX actually compile
  42)

(define (run-compiled-p cp)
  ;; XXX call out to the os to run the binary
  cp)

;; compile&run : prog -> ans
(define (compile&run p)
  (run-compiled-p (compile-p p)))

;; testing infrastructure
(module+ test
  (define (test-compiler p)
    (chk (compile&run p) (interp p)))
  (define t test-compiler)
  
  (t ex1)
  (t (:program (:num 42)))
  (t (:program (:- (:num 42)))))
