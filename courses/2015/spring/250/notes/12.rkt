#lang racket/base
(require racket/match
         racket/list
         (for-syntax racket/base
                     racket/list
                     syntax/parse))

(struct id (name) #:transparent)
(define (slack? i)
  (eq? #f (id-name i)))

(define (linear-print lin port mode)
  (when mode (write-string "<linear:" port))
  (define recur
    (case mode
      [(#t) write]
      [(#f) display]
      [else (lambda (p port) (print p port mode))]))
  (recur `(+ ,@(for/list ([(var coeff) (in-hash (linear-var->coeff lin))])
                 `(* ,coeff ,(id-name var)))
           ,(linear-constant lin))
         port)
  (when mode (write-string ">" port)))

(struct linear (var->coeff constant)
        #:methods gen:custom-write
        [(define write-proc linear-print)])
(define linear0 (linear (hasheq) 0))

(define (linears->vars lins)
  (sort (hash-keys (linear-var->coeff (foldl linear+ linear0 lins)))
        <=
        #:key (λ (v) (if (slack? v) 1 0))))

(define (linear->vector var-order lin)
  (match-define (linear var->coeff constant) lin)
  (for/vector #:length (length var-order)
              ([var (in-list var-order)])
              (hash-ref var->coeff var 0)))

(define (linears->coeff-matrix var-order lins)
  (for/vector #:length (length lins)
              ([lin (in-list lins)])
              (linear->vector var-order lin)))

(define (linears->constant-vector var-order lins)
  (for/vector #:length (length lins)
              ([lin (in-list lins)])
              (* -1 (linear-constant lin))))

;; This is wrong if the slack don't form a basis (in which case you
;; need to do a Phase I) this is the case when you use min, =, or >=
;; rather than only max and <=
(define (initial-tableau c A b)
  (define eqs (vector-length A))
  (define vars (vector-length c))
  (define mat (make-vector (+ eqs 1) #f))
  (for ([i (in-naturals)]
        [eq (in-vector A)])
    (define mat-eq (make-vector (+ vars 1) #f))
    (for ([j (in-naturals)]
          [e (in-vector eq)])
      (vector-set! mat-eq j e))
    (vector-set! mat-eq vars (vector-ref b i))
    (vector-set! mat i mat-eq))
  (define mat-last (make-vector (+ vars 1) #f))
  (for ([i (in-naturals)]
        [c (in-vector c)])
    (vector-set! mat-last i c))
  (vector-set! mat-last vars 0)
  (vector-set! mat eqs mat-last)
  mat)

(define (can-improve? vars t)
  (for/or ([e (in-vector
               (vector-ref t
                           (- (vector-length t)
                              1)))]
           [v (in-list vars)]
           [i (in-naturals)]
           #:unless (slack? v)
           #:when (positive? e))
    i))

(define (more-than-one-minimum ns)
  (cond
   [(= (length ns) 1)
    #f]
   [else
    (define 1st-min (apply min ns))
    (define ns-p (remove 1st-min ns))
    (define 2nd-min (apply min ns-p))
    (= 1st-min 2nd-min)]))

(define (find-pivot-index t col)
  (when
      (for/and ([row (in-vector t)])
        (not (positive? (vector-ref row col))))
    (error 'find-pivot-index "Unbounded"))

  (define quotients
    (for/list ([i (in-range (- (vector-length t) 1))]
               [r (in-vector t)]
               #:when (positive? (vector-ref r col)))
      (cons i (/ (vector-ref r (- (vector-length r) 1))
                 (vector-ref r col)))))

  (when (more-than-one-minimum (map cdr quotients))
    (error 'find-pivot-index "Degenerate"))

  (define row
    (car (argmin cdr quotients)))

  row)

(define (pivot-about t pi pj)
  (printf "\tpi = ~v, pj = ~v\n" pi pj)
  (define denom
    (vector-ref (vector-ref t pi) pj))
  (printf "\tdenom = ~v\n" denom)
  (define unit-pivot
    (let ()
      (define r (vector-ref t pi))
      (for/vector #:length (vector-length r)
                  ([x (in-vector r)])
                  (/ x denom))))
  (printf "\tunit-pivot = ~v\n" unit-pivot)
  (for/vector
      #:length (vector-length t)
      ([r (in-vector t)]
       [k (in-naturals)])
      (cond
       [(= pi k) unit-pivot]
       [else
        (for/vector
            #:length (vector-length r)
            ([x (in-vector r)]
             [up (in-vector unit-pivot)])
            (define y (* up (vector-ref r pj)))
            (- x y))])))

(define (extract-solution vars t)
  (for/fold ([subst (hasheq)])
            ([v (in-list vars)]
             [i (in-naturals)]
             #:unless (slack? v))
    (define val
      (for/or ([r (in-vector t)]
               #:when
               (and (= (vector-ref r i) 1)
                    (for/and ([ov (in-list vars)]
                              [oi (in-naturals)]
                              #:unless (eq? v ov)
                              #:unless (slack? ov))
                      (= (vector-ref r oi) 0))))
        (vector-ref r (length vars))))
    (if val
        (hash-set subst v val)
        subst)))

(define (simplex* cs obj)
  (define vars (linears->vars (cons obj cs)))
  (printf "VARS = ~v\n" vars)
  (define c (linear->vector vars obj))
  (printf "c = ~v\n" c)
  (define A (linears->coeff-matrix vars cs))
  (printf "A = ~v\n" A)
  (define b (linears->constant-vector vars cs))
  (printf "b = ~v\n" b)
  (define itab (initial-tableau c A b))
  (printf "itab = ~v\n" itab)
  (define final-tableau
    (let loop ([tableau itab])
      (printf "\n")
      (printf "\ttab = ~v\n" tableau)
      (define pivot-column
        (can-improve? vars tableau))
      (printf "\tpivot-col = ~v\n" pivot-column)
      (cond
       [pivot-column
        (loop (pivot-about
               tableau
               (find-pivot-index tableau
                                 pivot-column)
               pivot-column))]
       [else
        tableau])))
  (extract-solution vars final-tableau))

(module+ test
  (define x_1 (id 'x_1))
  (define x_2 (id 'x_2))
  (define slack_1 (id #f))
  (define slack_2 (id #f))
  (simplex*
   (list
    ;; 1 * x_1 + 2 * x_2 + 1 * slack_1 + (-4) = 0
    (linear (hasheq x_1 1
                    x_2 2
                    slack_1 1)
            -4)
    ;; 1 * x_1 + -1 * x_2 + 1 * slack_2 + (-1) = 0
    (linear (hasheq x_1 1
                    x_2 -1
                    slack_2 1)
            -1))
   ;; MAX: 3 * x_1 + 2 * x_2 + 0
   (linear (hasheq x_1 3 x_2 2)
           0)))

;; Turn constraints into symbolic linear equations

(define (linear+ lhs rhs)
  (match-define (linear lhs-map lhs-k) lhs)
  (match-define (linear rhs-map rhs-k) rhs)
  (linear (for/fold ([var->coeff lhs-map])
                    ([(var coeff) (in-hash rhs-map)])
            (hash-update var->coeff var
                         (λ (old) (+ old coeff))
                         0))
          (+ lhs-k rhs-k)))
(define (linear* sc l)
  (match-define (linear var->coeff k) l)
  (linear (for/hasheq ([(var coeff) (in-hash var->coeff)])
            (values var (* sc coeff)))
          (* sc k)))

(define (->linear e)
  (match e
    [(? number?)
     (linear (hasheq) e)]
    [(? id?)
     (linear (hasheq e 1) 0)]
    [(? linear?)
     e]))

(define (constraint+ lhs rhs)
  (match* (lhs rhs)
    [((? linear?) (? linear?))
     (linear+ lhs rhs)]
    [(_ _)
     (constraint+ (->linear lhs)
                  (->linear rhs))]))
(define (constraint* lhs rhs)
  (match* (lhs rhs)
    [((? number? coeff) (? id? var))
     (linear (hasheq var coeff) 0)]
    [((? id? var) (? number? coeff))
     (linear (hasheq var coeff) 0)]
    [((? number?) (? number?))
     (* lhs rhs)]
    [((? number? scalar) (? linear? lin))
     (linear* scalar lin)]
    [((? linear? lin) (? number? scalar))
     (linear* scalar lin)]
    [(_ _)
     (error 'constraint* "~v ~v" lhs rhs)]))
(define (constraint- lhs rhs)
  (constraint+ lhs (constraint* -1 rhs)))

(define-syntax (stx-pair-it stx)
  (syntax-parse stx
    [(_ fun arg1 arg2)
     (syntax/loc stx
       (fun arg1 arg2))]
    [(_ fun arg1 arg ...)
     (syntax/loc stx
       (fun arg1 (stx-pair-it fun arg ...)))]))

(define-syntax (compute-constraint-expr stx)
  (syntax-parse stx
    #:literals (+ - *)
    [(_ (+ arg ...))
     (syntax/loc stx
       (stx-pair-it constraint+ (compute-constraint-expr arg) ...))]
    [(_ (- arg ...))
     (syntax/loc stx
       (stx-pair-it constraint- (compute-constraint-expr arg) ...))]
    [(_ (* arg ...))
     (syntax/loc stx
       (stx-pair-it constraint* (compute-constraint-expr arg) ...))]
    [(_ var:id)
     #'var]
    [(_ coeff:number)
     #'coeff]))

(define-syntax (compute-constraint stx)
  (syntax-parse stx
    #:literals (=)
    [(_ (= lhs rhs))
     (syntax/loc stx
       (compute-constraint-expr (- lhs rhs)))]))

(define-syntax (normalize-constraint stx)
  (syntax-parse stx
    #:literals (>= <= =)
    [(_ (~and con (= . _)))
     (quasisyntax/loc #'con
       (compute-constraint con))]
    ;; x <= y -----> x + e = y
    [(_ (~and con (<= lhs rhs)))
     (quasisyntax/loc stx
       (let ([slack (id #f)])
         (normalize-constraint
          #,(quasisyntax/loc #'con
              (= (+ lhs slack) rhs)))))]
    ;; x >= y -----> x - e = y
    [(_ (~and con (>= lhs rhs)))
     (quasisyntax/loc stx
       (let ([slack (id #f)])
         (normalize-constraint
          #,(quasisyntax/loc #'con
              (= (- lhs slack) rhs)))))]))

(define-syntax (normalize-objective stx)
  (syntax-parse stx
    #:literals (max min)
    [(_ (max obj))
     (syntax/loc stx
       (compute-constraint-expr obj))]
    [(_ (min obj))
     (syntax/loc stx
       (normalize-objective (max (* -1 obj))))]))

(define-syntax (simplex stx)
  (syntax-parse stx
    [(_ (var:id ...) constraint:expr ... objective:expr)
     (syntax/loc stx
       (let ()
         (define var (id 'var))
         ...
         (simplex*
          (list (normalize-constraint constraint)
                ...)
          (normalize-objective objective))))]))

;; Some tests

(module+ test
  (simplex (x_1 x_2)
           (<= (+ x_1 (* 2 x_2))
               4)
           (<= (- x_1 x_2)
               1)
           (max
            (+ (* 3 x_1)
               (* 2 x_2)))))

(module+ test-general
  (simplex (x_1 x_2)
           (>= (+ x_1 (* 2 x_2))
               2)
           (>= (- x_1 x_2)
               1)
           (min
            (+ (* 3 x_1)
               (* 2 x_2))))

  (simplex (b m r)
           (>= (+ (* 91 b) (* 87 m) (* 87 r))
               3700)
           (>= (+ (* 47 b) (* 276 m) (* 40 r))
               1000)
           (>= (+ (* 89.2 b) (* 0 m) (* 53.2 r))
               90)
           (min (+ (* 0.381 b)
                   (* 0.1 m)
                   (* 0.272 r))))
  "should be b -> 1.01, m -> 41.47, r -> 0"

  (simplex (x1 x2 x3)
           (>= (+ x1 x2 x3) 6)
           (>= (+ (* 2 x1) x3) 2)
           (>= (+ x2 x3) 1)
           (min (+ (* 4 x1) (* 3 x2) (* 9 x3))))

  (simplex (x1 x2 x3)
           (<= (- (* 3 x1) (* 2 x2)) 7)
           (>= (+ x2 (* 4 x3)) 10)
           (= (+ x1 x2) 2)
           (min
            (+ x1 x2 x3)))

  (simplex (x y)
           (>= x 2)
           (>= y 2)
           (<= x 10)
           (<= y 10)
           (max (+ x y))))
