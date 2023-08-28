#lang racket
(require (prefix-in original: racket))
(original:if 1 2 3)

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
  (let* ([start (current-milliseconds)]
         [ans (thunk)]
         [end (current-milliseconds)])
    (printf "Took ~ams\n" (- end start))
    ans))

(time-it* (λ () (fib 13)))
(time-it* (λ () (fib 24)))

(define-syntax-rule (time-it expr)
  (time-it* (λ () expr)))

(time-it (fib 13))
(time-it (fib 24))

(let ([x 4]) (+ x x))

(define-syntax my-let
  (syntax-rules ()
    [(my-let ([id expr]
              ...)
       body
       ...)
     ((lambda (id ...)
        body ...)
      expr ...)]))

(my-let ([x 4]) (+ x x))
(my-let ([x 4] [y 5]) (+ x y))

(my-let ([x 5])
        (my-let ([x 4] [y x]) 
                (+ x y)))

(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body ...)
     (begin body ...)]
    [(my-let* ([fst-id fst-expr]
               [rst-id rst-expr]
               ...)
              body ...)
     
     ((lambda (fst-id)
        (my-let* ([rst-id rst-expr]
                  ...)
                 body ...))
      fst-expr)]))

(my-let* ([x 4] [y x]) 
         (+ x y))

(cond [(even? 2) 'even]
      [else 'odd])
      

(define-syntax cond2
  (syntax-rules (else)
    [(cond2 [true-test? true-expr]
            [else false-expr])
     (if true-test?
         true-expr
         false-expr)]))

(cond2 [(even? 2) 'even]
       [else 'odd])

#;(cond2 [(even? 2) 'even]
         [frozzle 'odd])

(or 'foo #f)
(or #f 'bar)
(or #f #f)

(define-syntax or2
  (syntax-rules ()
    [(or2 fst snd)
     (my-let ([tmp fst])
             (if tmp 
                 tmp
                 snd))]))

(or2 'foo #f)
(or2 #f 'bar)
(or2 #f #f)
(or2 (time-it (fib 4)) #f)
     
(let ([tmp 79])
  (or2 #f tmp))

(or2 #f (or2 'first #f))

(let ([if (λ (x y z) (+ x y z))])
  (or2 1 2))

(define-syntax my-for
  (syntax-rules (from to)
    [(my-for iter-id from start to end body ...)
     (local [(define end-val end)
             (define (the-loop iter-id)
               (when (iter-id . < . end-val)
                 body ...
                 (the-loop (add1 iter-id))))]
       (the-loop start))]))

(my-for x
        from 0
        to (time-it (fib 3))
        (printf "~a" x))

(define-syntax (my-for2 stx)
  (syntax-case stx (from to)
    [(my-for2 from start to end body ...)
     (with-syntax ([iter-id (datum->syntax stx 'it)])
       (syntax/loc stx
         (local [(define end-val end)
                 (define (the-loop iter-id)
                   (when (iter-id . < . end-val)
                     body ...
                     (the-loop (add1 iter-id))))]
           (the-loop start))))]))

(define it 2)
(my-for2 
 from 0
 to (time-it (fib 3))
 (printf "~a" it))

;; DSL

#|
machine start
 start : c -> init
 init  : a -> more
         d -> more
 more  : a -> more
         d -> more
         r -> end
 end

car
cdr
cadr
cddar
cddddar
cddddr
caar
|#

#|
(define cadr-machine
  '(machine start
          ([start ([c -> init])]
           [init ([a -> more]
                  [d -> more])]
           [more ([a -> more]
                  [d -> more]
                  [r -> end])]
           [end ()])
          end))

(define (accepts? m i)
  (define (loop s i)
    (if (empty? i)
        (symbol=? (fourth m) s)
        (with-handlers ([exn:fail? (λ (x) #f)])
          (loop (third (assq (first i) (second (assq s (third m)))))
                (rest i)))))
  (loop (second m) i))
|#

#;(define (start i)
  (if (empty? i)
      #f
      (case (first i)
        [(c) (init (rest i))]
        [else #f])))

(define-syntax-rule 
  (machine start-state
           ([state ([char -> next-state]
                    ...)]
            ...)
           final-state)
  (λ (input)
    
    (define (state i)
      (if (empty? i)
          #f
          (case (first i)
            [(char) (next-state (rest i))]
            ...
            [else #f])))
    ...
    
    (define (final-state i)
      (empty? i))
    
    (start-state input)))

(define cadr-machine
  (machine start
           ([start ([c -> init])]
            [init ([a -> more]
                   [d -> more])]
            [more ([a -> more]
                   [d -> more]
                   [r -> end])])
           end))

(define (accepts? m i)
  (m i))

(require plai)

(test (accepts? cadr-machine '(c a r)) #t)
(test (accepts? cadr-machine '(c a d r)) #t)
(test (accepts? cadr-machine '(c a d a r)) #t)
(test (accepts? cadr-machine '(c a d a r r)) #f)
(test (accepts? cadr-machine '(c a d a)) #f)
(test (accepts? cadr-machine '(a d a r)) #f)

