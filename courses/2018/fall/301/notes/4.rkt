;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Lambda Calculus Exprs =

;;    X (variable reference)
;; Racket: X
;; Javascript: X

;; λX.E (abstraction i.e. a function)
;; Racket: (λ (X) E)
;; JS: function (X) { return E; }

;;  M N (application)
;; Racket: (M N)
;; JS: M(N)

(define TRUE (λ (t) (λ (f) t)))
(define FALSE (λ (t) (λ (f) f)))
(define pair (λ (l) (λ (r) (λ (c) ((c l) r)))))
(define fst (λ (p) (p TRUE)))
(define snd (λ (p) (p FALSE)))

;; LC: zero := λf. λz. z
(define zero (λ (f) (λ (z) z)))
;; JS: function zero (f) { return function (z) { return z; } }

(define succ
  (λ (n)
    (λ (f)
      (λ (z)
        (f ((n f) z))))))

(define one (succ zero))

(((snd ((pair zero) one)) add1) 0) ;; should 1

#;(define one (λ (f) (λ (z) (f z))))

(define plus
  (λ (x)
    (λ (y)
      (λ (f)
        (λ (z)
          ((y f) ((x f) z)))))))

(define two ((plus one) one))
(define four ((plus two) two))

four

((four add1) 0)

#;(snd ((four fun-that-add1s-and-doesnt-add1s) (pair 0 0)))

((lambda (f)
   (lambda (z)
     (((lambda (f) (lambda (z) (f z))) f)
      (((lambda (f) (lambda (z) (f z))) f)
       z))))
 add1)

;; Infinite Loop
#;((λ (x) (x x))
   (λ (x) (x x)))