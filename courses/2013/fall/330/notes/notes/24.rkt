#lang plai
(require (for-syntax racket/list))

;; Desugaring

;; - You some um syntax that means nothing to the core of the langauge
;; but allows the users to do something in a more useful way and you
;; render it down to the core language.

;; If you have + and *, then you don't need -, "x - y" = "x + -1*y"

;; Allows you to shrink the core OR grow the human syntax

(test (let ([x 5]) (+ x x))
      10)
(test ((λ (x) (+ x x)) 5)
      10)

(define (f x y)
  (+ x y))

(define-syntax-rule
  ;; input:
  (with ([bound-id bound-expr]) with-body-expr)
  ;; output:
  ((λ (bound-id) with-body-expr) bound-expr))

(test (with ([x 5]) (+ x x))
      10)

;; We should be ashamed...
;; (with ([4 5]) (+ x x))
;; Our abstraction leaked

(define-syntax-rule
  ;; input:
  (wwith bound-id bound-expr with-body-expr)
  ;; output:
  ((λ (bound-id) with-body-expr) bound-expr))

(test (wwith x 5 (+ x x))
      10)

(define-syntax-rule
  ;; input:
  (mwith ([bound-id1 bound-expr1] ;; bind the-pattern
          ...) body-expr)
  ;; output:
  ;; (map first the-pattern)
  ((λ (bound-id1 ...) body-expr)
   ;; (map second the-pattern)
   bound-expr1 ...))

(test (mwith ([x 5] [y 7]) (+ x y x))
      17)

;; Shame on us
;; (mwith ([x 5] [x 7]) (+ x x))

;; macro is a function from stx to stx
(define-syntax (mwith2 stx)
  (syntax-case stx ()
    [(mwith2 ([bound-id bound-expr]
              ...) body-expr)
     (cond
       ;; (andmap f l) ==
       ;;  (foldr (λ (x a) (and (f x) a)) true l)
       [(not (andmap identifier?
                     (syntax->list
                      (syntax (bound-id ...)))))
        (syntax (error 'mwith2 "not a symbol"))]
       [(not (equal?
              (map syntax->datum
                   (syntax->list
                    (syntax (bound-id ...))))
              (remove-duplicates
               (map syntax->datum
                    (syntax->list
                     (syntax (bound-id ...)))))))
        (syntax (error 'mwith2 "duplicate"))]
       [else
        (syntax ((λ (bound-id ...) body-expr)
                 bound-expr ...))])]))

(test/exn (mwith2 ([4 5] [y 7]) (+ x y x))
          "not a symbol")
(test/exn (mwith2 ([x 5] [x 7]) (+ x y x))
          "duplicate")
(test (mwith2 ([x 5] [y 7]) (+ x y x))
      17)

(define-syntax (crazy-macro stx)
  (datum->syntax stx (random 100)))

(crazy-macro 55)

(define-syntax-rule
  (my-or1 fst snd)
  (if fst fst snd))

(test (my-or1 #f #t) #t)
(test (my-or1 #f #f) #f)
(test (my-or1 #t #f) #t)
(test (my-or1 #t #t) #t)

(define-syntax my-or2
  (syntax-rules ()
    [(my-or2)
     #f]
    [(my-or2 fst snd ...)
     (if fst fst (my-or2 snd ...))]))

(test (my-or2 #f #f #t) #t)

(test (let ([v #f])
        (my-or2 v 3))
      3)

(test (let ([v #f])
        (my-or2 (begin (set! v (not v)) v) 3))
      #t)

(test (let ([v #f])
        (if (begin (set! v (not v)) v)
          (begin (set! v (not v)) v)
          3))
      #t)

(define-syntax my-or3
  (syntax-rules ()
    [(my-or3)
     #f]
    [(my-or3 fst snd ...)
     (with ([tmp fst])
           (if tmp tmp (my-or3 snd ...)))]))

(test (let ([v #f])
        (my-or3 (begin (set! v (not v)) v) 3))
      #t)

(test (let ([v #f])
        (let ([tmp (begin (set! v (not v)) v)])
          (if tmp tmp 3)))
      #t)

(test (let ([tmp #f])
        (my-or3 (begin (set! tmp (not tmp)) tmp) 3))
      #t)

(test (let ([tmp #f])
        (let ([tmp (begin (set! tmp (not tmp)) tmp)])
          (if tmp tmp 3)))
      #t)

(test (let ([tmp 5])
        (my-or3 #f tmp))
      5)

(test (let ([tmp 5])
        (let ([tmp #f]) 
          (if tmp tmp tmp)))
      #f)

(test (let ([blue-tmp 5])
        (let ([red-tmp #f]) 
          (if red-tmp red-tmp blue-tmp)))
      5)
