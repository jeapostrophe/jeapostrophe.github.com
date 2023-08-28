#lang racket

;; Mao's Last Dancer

;; A language is...
;; --- notation
;; --- natural data
;; --- natural control constructs
;; --- organizing your code... so modules, functions, classes, stuff like that
;; --- automation of somethings... so memory management, continuations, safety (i.e. types)

;; A domain-specific language... (general purpose... C, Java, Racket)
;; --- anything of these may be the focus of the DSL
;; --- to do it, its needs to make some restrictions

;; How do we implement a DSL?
;; --- Traditional path: Parser -> Interpreter/Compiler -> Standard Libraries... is expensive
;; --- Racket is designed to make DSL creation easy

;; A programming language should be designed not by piling feature
;; ontop of feature, but removing the restrictions and inconsistencies
;; that make new features appear necessary.

;; (for ([i (in-range 10)])
;;   ...)
;; =>
;; (define (this-for x)
;;   ...)

(define (time-it* f)
  (define start (current-inexact-milliseconds))
  (displayln (f))
  (define end (current-inexact-milliseconds))
  (- end start))

(define-syntax-rule (time-it e)
  (time-it* (λ () e)))

;; #define foo(x) (x     // <---- not syntactically correct

(time-it (expt 2 5000))
(time-it (expt 2 10000))
"Done"

;; #define foo(x) do { x } while (0)

;; if (...) foo(x)

(define-syntax-rule (with ([bound-id bound-body]) body)
  ((λ (bound-id) body) bound-body))

(with ([x (+ 1 2)])
      (display (+ x 6)))

;; (with* ([bound-id bound-body]
;;         ...)
;;        body)

(define-syntax-rule (naive-with* ([bound-id bound-body] ...) body)
  ((λ (bound-id ...) body) bound-body ...))

(define-syntax with*
  (syntax-rules ()
    [(with* () body)
     body]
    [(with* ([bound-id_0 bound-body_0]
             [bound-id_n bound-body_n]
             ...)
            body)
     (with ([bound-id_0 bound-body_0])
           (with* ([bound-id_n bound-body_n]
                   ...)
                  body))]))

(with* ([x 5]
        [y 7]
        [z (+ x y)])
       (list x y z))

;; Prolog assignment help, over, yeah!

(define-syntax jond
  (syntax-rules (else)
    [(jond [question answer] [else else-answer])
     (if question answer else-answer)]))

(jond
 [(even? 2) "even"]
 [else "odd"])
(jond
 [(even? 3) "even"]
 [else "odd"])

;; (jond
;;  [(even? 3) "even"]
;;  [(/ 1 0) "odd"])

;; Macros have static scope

(with ([time-it* 7])
      (time-it (length empty)))

(define-syntax-rule (jor lhs rhs)
  (with ([lhs-v lhs])
        (if lhs-v lhs-v rhs)))

(jor #t (/ 1 0))

(jor (displayln "hey, listen") (/ 1 0))

;; Hygeine
(with ([lhs-v 44])
      (jor #f lhs-v))

(define-syntax (for stx)
  (printf "I'm a function! ~a\n" stx)
  (define x (read))
  (printf "You typed ~a at compile-time.\n" x)
  (syntax-case stx (from to do)
    [(_ from start to end do body ...)
     (with-syntax
         ([it (datum->syntax stx 'it)])
       (syntax
         (let loop ([it start])
           (unless (= it end)
             body ...
             (loop (add1 it))))))]))

(define it 15)
(for from 7 to 10 do (displayln it))
