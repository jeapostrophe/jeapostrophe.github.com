#lang racket

(define (fib n)
  (if (n . <= . 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

#|

(define start (current-milliseconds))
(fib 13)
(define end (current-milliseconds))
(- end start)

(define start0 (current-milliseconds))
(fib 24)
(define end0 (current-milliseconds))
(- end0 start0)

|#

(define (time-it* thunk)
  (let* ([start (current-inexact-milliseconds)]
         [ans (thunk)]
         [end (current-inexact-milliseconds)])
    (printf "Took ~ams\n" (- end start))
    ans))

(time-it* (λ () (fib 13)))
(time-it* (λ () (fib 24)))

(define-syntax-rule (time-it expr)
  (time-it* (λ () expr)))

(time-it (fib 13))
(time-it (fib 24))

#;(+ x x)

(define-syntax-rule (with ([id expr]) body ...)
  ((lambda (id) body ...)
   expr))

(with ([x 4]) (+ x x))

(define-syntax-rule (with* ([id expr] ...) body ...)
  ((lambda (id ...) body ...)
   expr ...))

(with* ([x 4] [y 5]) (+ x y))

(define-syntax with**
  (syntax-rules ()
    [(with** () body ...)
     (begin body ...)]
    [(with** ([id0 expr0] [id expr] ...) body ...)
     (with ([id0 expr0])
       (with** ([id expr] ...)
         body ...))]))

(with** ([x 4] [y x]) (+ x y))

(cond [(even? 2) 'even]
      [else 'odd])

(define-syntax cond2
  (syntax-rules (else)
    [(cond2 [test true-expr]
            [else false-expr])
     (if test
         true-expr
         false-expr)]))

(cond2 [(even? 2) 'even]
       [else 'odd])

#;(cond2 [(even? 3) 'even]
         [(/ 1 0) 'odd])

(or 'foo 'bar)
(or 'foo #f)
(or #f 'bar)
(or #f #f)

(define-syntax-rule (or2 fst snd)
  (with ([tmp fst])
    (if tmp
        tmp
        snd)))

(or2 'foo 'bar)
(or2 'foo #f)
(or2 #f 'bar)
(or2 #f #f)

(or2 (time-it (fib 13)) #f)
(or2 #f (time-it (fib 13)))

(with ([tmp 5])
  (or2 #f tmp))

(with ([if (λ (x y z) (+ x y z))])
  (or2 1 2))

(define-syntax my-for
  (syntax-rules (step from to)
    [(my-for iter-id step size from start to end body ...)
     (local [(define size-val size)
             (define end-val end)
             (define (the-loop iter-id)
               (when (iter-id . < . end-val)
                 body ...
                 (the-loop (+ size-val iter-id))))]
       (the-loop start))]))

(my-for x
        step 2
        from 0
        to (time-it (fib 5))
        (printf "~a" x))

(define-syntax (my-for2 stx)
  (syntax-case stx (step from to)
    [(my-for2 step size from start to end body ...)
     (with-syntax
         ([iter-id (datum->syntax stx 'it)])
       (syntax/loc stx
         (local [(define size-val size)
                 (define end-val end)
                 (define (the-loop iter-id)
                   (when (iter-id . < . end-val)
                     body ...
                     (the-loop (+ size-val iter-id))))]
           (the-loop start))))]))

(my-for2 step 2
         from 0
         to (time-it (fib 5))
         (printf "~a" it))