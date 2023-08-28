#lang scheme

; Measuring Time 
;; Try one
(define (my-time1 e)
  (define begin-time (current-milliseconds))
  e
  (- (current-milliseconds) begin-time))

(my-time1 (+ 1 2))
(my-time1 (expt 2 1000))
(my-time1 (expt 2 10000))
(my-time1 (expt 2 100000000))

;; Try two
(define (my-time2 e-thunk)
  (define begin-time (current-milliseconds))
  (e-thunk)
  (- (current-milliseconds) begin-time))

(my-time2 (lambda () (+ 1 2)))
(my-time2 (lambda () (expt 2 1000)))
(my-time2 (lambda () (expt 2 10000)))
(my-time2 (lambda () (expt 2 100000000)))

;; Try three
(define-syntax my-time3
  (syntax-rules ()
    [(my-time3 e)
     (local [(define begin-time (current-milliseconds))]
       e
       (- (current-milliseconds) begin-time))]))

(my-time3 (+ 1 2))
(my-time3 (expt 2 1000))
(my-time3 (expt 2 10000))
(my-time3 (expt 2 100000000))

; Local Definitions
(define-syntax my-let-1
  (syntax-rules ()
    [(my-let-1 (var val) body)
     ((lambda (var) body) val)]))

(my-let-1 (x 3) (+ x 4))
(my-let-1 (x 3) (my-let-1 (x 7) (+ x 4)))

(define-syntax my-let
  (syntax-rules ()
    [(my-let ([var val] ...) body)
     ((lambda (var ...) body) val ...)]))

(my-let ([x 3] [y 7]) (+ x y))

; Nested Local Definitions
(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body)
     body]
    [(my-let* ([var0 val0]
               [var-rest val-rest] ...)
              body)
     (let ([var0 val0])
       (my-let* ([var-rest val-rest] ...)
                body))]))

(my-let* ([a 0]
          [b 2]
          [x (+ a b)]
          [y (+ a x)]
          [z (* x y)])
         z)

; Simple Conditional
;; Try 1
(define-syntax cond2
  (syntax-rules ()
    [(cond2 (t e1) (else e2))
     (if t e1 e2)]))

(cond2 [(even? (current-seconds)) 'even]
       [else 'odd])

(cond2 [(even? (current-seconds)) 'even]
       [(odd? (current-seconds)) 'odd])

;; Try 2
(define-syntax cond2+
  (syntax-rules (else)
    [(cond2+ (t e1) (else e2))
     (if t e1 e2)]))

(cond2+ [(even? (current-seconds)) 'even]
        [else 'odd])

; Errors
#;(cond2+ [(even? (current-seconds)) 'even]
          [(odd? (current-seconds)) 'odd])

; Disjunction
;; Try 1
(define (my-or2-fun e1 e2)
  (if e1 e1 e2))

(my-or2-fun false true)

; Errors
#;(let ([x 0])
    (my-or2-fun (zero? x) (zero? (/ 1 x))))

;; Try 2
(define-syntax my-or2
  (syntax-rules ()
    [(my-or2 e1 e2)
     (if e1 e1 e2)]))

(my-or2 false true)

(let ([x 0])
  (my-or2 (zero? x) (zero? (/ 1 x))))

;; Try 3
(define-syntax my-or2+
  (syntax-rules ()
    [(my-or2+ e1 e2)
     (let ([result e1])
       (if result result e2))]))

(let ([x 0])
  (my-or2+ (zero? x) (zero? (/ 1 x))))

; Hygeine
(let ([result true])
  (my-or2+ false result))

;; Loops

; Try 1
(define-syntax for0
  (syntax-rules (from to in)
    [(for0 var from low to high in bodies ...)
     (local [(define loop (lambda (var)
                            (if (> var high)
                                'done
                                (begin bodies ...
                                       (loop (+ var 1))))))]
       (loop low))]))

(for0 x
      from 2
      to 5
      in (display x))

; Annoying
#;(for0 x
        from 2
        to (read)
        in (display x))

; Try 2
(define-syntax for1
  (syntax-rules (from to in)
    [(for1 var from low to high in bodies ...)
     (local [(define high-val high)
             (define loop (lambda (var)
                            (if (> var high-val)
                                'done
                                (begin bodies ...
                                       (loop (+ var 1))))))]
       (loop low))]))


(for1 x
      from 2
      to 5
      in (display x))

#;(for1 x
        from 2
        to (read)
        in (display x))

; Try 3
(define-syntax for2
  (syntax-rules (from to in)
    [(for2 from low to high in bodies ...)
     (local [(define high-val high)
             (define loop (lambda (it)
                            (if (> it high-val)
                                'done
                                (begin bodies ...
                                       (loop (+ it 1))))))]
       (loop low))]))

; Errors
#;(for2 from 2
        to 5
        in (display it))

; Try 4
(define-syntax (for3 stx)
  (syntax-case stx (from to in)
    [(for3 from low to high in bodies ...)
     (with-syntax ([it (datum->syntax stx 'it)])
       (syntax
        (local [(define high-val high)
                (define loop (lambda (it)
                               (if (> it high-val)
                                   'done
                                   (begin bodies ...
                                          (loop (+ it 1))))))]
          (loop low))))]))

(for3 from 2
      to 5
      in (display it))

(for3 from 2 to 5 in
      (for3 from 1 to it in
            (display it))
      (newline))