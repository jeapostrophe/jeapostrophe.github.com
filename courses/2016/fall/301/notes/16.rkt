#lang racket/base
(require (for-syntax racket/base))

#;(define (withC name named-expr body)
    (appC (lamC name body) named-expr))

;; x2 5 = 10
;; int double(int x) { return x + x ; }

(define-syntax-rule
  (with name named-expr body)
  ((lambda (name) body)
   named-expr))

(with x 5
      (+ x x))

(define
  (with-fun name named-expr body)
  ((lambda (name) body)
   named-expr))
#; ;; Doesn't work because x is unbound
(with-fun x 5
  (+ x x))

#|
int maybe_lm (int i ) {
return (i-- && launch_missiles());
return (--i && launch_missiles());
}

maybe_lm(5)
maybe_lm(1)

#define EXAMPLE_MACRO foo(

|#

;; jay-or : syntax -> syntax
(define-syntax (jay-or stx)
  (syntax-case stx ()
    [(_ lhs rhs)
     (begin
       (printf "I am happening at compile time\n")
       (syntax
         (if lhs
           lhs
           rhs)))]))

(define (launch-missiles)
  (printf "Sorry, Russia\n"))

(jay-or #f "rhs")
(jay-or #t (launch-missiles))

(define-syntax (compile-time-fac stx)
  (syntax-case stx ()
    [(_ n)
     (let ()
       (define (fac i)
         (printf "fac of ~v\n" i)
         (if (zero? i)
           1
           (* i (fac (sub1 i)))))
       (datum->syntax
        stx
        (fac
         (syntax->datum #'n))))]))

(compile-time-fac 5)

;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (jay-or2 stx)
  (syntax-case stx ()
    [(_)
     (syntax
       #f)]
    ;; Might be a good idea so the compiler has less work to do
    #;[(_ lhs)
       (syntax
         lhs)]
    [(_ lhs rhs ...)
     (syntax
       (if lhs ;; <-- compiled and eval'd once
         lhs ;; <-- compiled and eval'd twice
         (jay-or2 rhs ...)))
     (syntax
       (with lhs-v lhs
             (if lhs-v
               lhs-v
               (jay-or2 rhs ...))))]))

"jay-or2"
(jay-or2 (> 1 5) (> 4 6) "rhs")
(jay-or2 #t (launch-missiles))

(jay-or2 #f (launch-missiles))
;; => shouldn't be =>
#;(if #f
    #f
    (if (launch-missiles)
      (launch-missiles)
      #f))

(let ([x 12])
  (jay-or2 #f x))

;; SHOULD be 12 <-------- no argument
;; WILL be 12   <-------- DEFAULT -- lexical "macro closures"
;; SHOULD be #f
;; WILL be #f   <-------- can do, but COMPLICATED
(let ([lhs-v 12])
  (jay-or2 #f lhs-v))

;; In most Lisp languages, the default is to return #f

;; This is called a "non-hygienic macro system"

;; The complicated way to get lexical scope is to "make an
;; unrepeatable name"

;; Anaphoric if
(define-syntax (ana-if stx)
  (syntax-case stx ()
    [(_ cond true false)
     (with-syntax ([it (datum->syntax stx 'it)])
       (syntax
         (let ([it cond])
           (if it
             true
             false))))]))

(ana-if (+ 5 9)
        (printf "The answer was true and was ~v\n"
                it)
        (printf "Not a true value"))
