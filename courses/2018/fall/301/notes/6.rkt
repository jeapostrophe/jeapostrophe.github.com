#lang racket/base
(require racket/match)
(module+ test
  (require chk))

;; ISWIM terms
;; M = X | b | λX.M | (M N) | (o M ...)
(struct Var (x) #:transparent)
(struct con (b) #:transparent)
(struct lam (x m) #:transparent)
(struct App (m n) #:transparent)
(struct prm (o ms) #:transparent)

(module+ test
  (define id (lam 'foo (Var 'foo)))
  (define seven (con 7))
  ;; ISWIM ex1 = new App(new Lam("foo",new Var("foo")), new Con(7));
  (define ex1 (App id seven))
  ex1)

;; V = λX.M | b
(define (V? x)
  (or (lam? x) (con? x)))

;; E = H | (E N) | (V E) | (o V ... E M ...)
(struct ec-hole () #:transparent)
(struct ec-fun (e n) #:transparent)
(struct ec-arg (v e) #:transparent)
(struct ec-prm (o vs e ms) #:transparent)

(define (plug ec m)
  (match ec
    [(ec-hole) m]
    [(ec-fun e n) (App (plug e m) n)]
    [(ec-arg v e) (App v (plug e m))]
    [(ec-prm o vs e ns)
     (prm o (append vs (list (plug e m)) ns))]))
(module+ test
  (chk (plug (ec-hole) id) id)
  (chk (plug (ec-fun (ec-hole) seven) id)
       ex1))

(struct some-work (e m))

(define (find-the-work p)
  (match p
    [(Var x) #f]
    [(con b) #f]
    [(lam x m) #f]
    [(App m n)
     (cond
       [(and (V? m) (V? n))
        (some-work (ec-hole) p)]
       [(V? m)
        (match (find-the-work n)
          [#f #f]
          [(some-work inner-e inner-p)
           (some-work (ec-arg m inner-e)
                      inner-p)])]
       [else
        (match (find-the-work m)
          [#f #f]
          [(some-work inner-e inner-p)
           (some-work (ec-fun inner-e n)
                      inner-p)])])]))

(define (subst x v p)
  (match p
    [(Var y) (if (eq? x y) v p)]
    [(con b) p]
    [(lam y m)
     (if (eq? x y)
       p
       (lam y (subst x v m)))]
    [(App m n)
     (App (subst x v m)
          (subst x v n))]))

(define (do-the-work p)
  (match p
    [(App (lam x m) v)
     (subst x v m)]))

(define (sr p)
  (printf "~v\n" p)
  ;; P = E[M]
  (match (find-the-work p)
    [#f
     (printf "\t=> no work\n")
     #f]
    [(some-work e m)
     ;; M v N
     (define n (do-the-work m))
     ;; E[N]
     (plug e n)]))

(define (sr* p)
  (define np (sr p))
  (if np
    (sr* np)
    p))

(module+ test
  (chk (sr* seven) seven)
  (chk (sr* ex1) seven))
