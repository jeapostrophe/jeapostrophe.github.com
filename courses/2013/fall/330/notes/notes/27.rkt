#lang plai
(print-only-errors #t)
(halt-on-errors #t)

(define (read-number prompt)
  (displayln prompt)
  (read))

(define (add-two-numbers.exe)
  (displayln (+ (read-number "First")
                (read-number "Second"))))

;; (add-two-numbers.exe)

(define dispatch-table (make-hash))
(define (new-label)
  (hash-count dispatch-table))

(define (read-number/and-then prompt and-then)
  (define label (new-label))
  (hash-set! dispatch-table label
             and-then)
  (printf "~a\nGo to ~a with answer\n"
          prompt
          label)
  (error 'end))

(define (firefox label answer)
  ((hash-ref dispatch-table label)
   answer))

(define (add-two-numbers.com)
  (read-number/and-then "First"
                        add-two-numbers.com/given-first))
(define (add-two-numbers.com/given-first first-num)
  (define (add-two-numbers.com/given-second second-num)
    (displayln (+ first-num
                  second-num)))
  (read-number/and-then "Second"
                        add-two-numbers.com/given-second))

;; (add-two-numbers.com)

;; CPS - ______ Passing Style --- "Control" Passing Style?

;; C stands for "Continuation"

;; The properities are....
;; - It can be done to /any/ program
;; - Calls exactly one function
;;   (f a)
;;   (g (f a))
;;  translated to
;;   (f^ a g)
;; - To get around one fucntion restriction, passes addt'l thing
;; - What's a good name... "and then", "the goto", "the callback"
;; - Calls exactly 1 = "does not return"

(define 1-cpsd
  (λ (return)
    (return 1)))

(define (run cpsd-program)
  (cpsd-program (λ (x) x)))

(test (run 1-cpsd) 1)

(define-syntax cpser
  (syntax-rules (quote let1 rec1 begin set! if λ displayln zero? read-number +)
    [(cpser (if cond-expr true-expr false-expr))
     (λ (return)
       ((cpser cond-expr)
        (λ (cond-val)
          (if cond-val
            ((cpser true-expr) return)
            ((cpser false-expr) return)))))]
    [(cpser (begin fst-expr snd-expr))
     (λ (return)
       ((cpser fst-expr)
        (λ (fst-value)
          ((cpser snd-expr) return))))]
    [(cpser (set! some-id val-expr))
     (λ (return)
       ((cpser val-expr)
        (λ (the-val)
          (set! some-id the-val)
          (return the-val))))]
    [(cpser (rec1 ([bound-id bound-expr]) body-expr))
     (cpser (let1 ([bound-id 0])
                  (begin (set! bound-id bound-expr)
                         body-expr)))]
    [(cpser (let1 ([bound-id bound-expr]) body-expr))
     (cpser ((λ (bound-id) body-expr) bound-expr))]
    [(cpser (quote some-quoted-value))
     (λ (return) (return 'some-quoted-value))]
    [(cpser (λ (some-id) body-expr))
     (λ (place-to-return-the-lambda-to)
       (place-to-return-the-lambda-to
        (λ (some-id some-place-to-return-the-answer-to)
          ((cpser body-expr) some-place-to-return-the-answer-to))))]
    [(cpser (displayln arg-expr))
     (λ (return)
       ((cpser arg-expr)
        (λ (arg-val)
          (return (displayln arg-val)))))]
    [(cpser (zero? arg-expr))
     (λ (return)
       ((cpser arg-expr)
        (λ (arg-val)
          (return (zero? arg-val)))))]
    [(cpser (read-number arg-expr))
     (λ (return)
       ((cpser arg-expr)
        (λ (arg-val)
          (read-number/and-then arg-val return))))]
    [(cpser (+ lhs-expr rhs-expr))
     (λ (return)
       ((cpser lhs-expr)
        (λ (lhs-val)
          ((cpser rhs-expr)
           (λ (rhs-val)
             (return (+ lhs-val rhs-val)))))))]
    [(cpser (fun-expr arg-expr))
     (λ (return)
       ((cpser fun-expr)
        (λ (fun-val)
          ((cpser arg-expr)
           (λ (arg-val)
             (fun-val arg-val return))))))]
    [(cpser some-value)
     (λ (return) (return some-value))]))

(test (run (cpser 1)) 1)
(test (run (cpser 'x)) 'x)
(test (run (cpser (let1 ([x 4]) x))) 4)
(test (run (cpser (rec1 ([x 4]) x))) 4)
;; It's really recursive, but we are jerks.
(test (run (cpser (rec1 ([x (+ 1 x)]) x))) 1)
(test (run (cpser (let1 ([x 4]) (begin (set! x 3) x)))) 3)
(test (run (cpser (if #t 1 2))) 1)
(test (run (cpser (if #f 1 2))) 2)
(test (run (cpser ((λ (x) x) 1))) 1)

(define (add-two-numbers.exe.com)
  (run
   (cpser
    (displayln
     (+ (read-number "First")
        (read-number "Second"))))))

;; (add-two-numbers.exe.com)

(define (add-many-numbers.exe.com)
  (run
   (cpser
    (rec1 ([ask-until-zero
            (λ (ignored)
              (let1 ([x (read-number "Gimme another")])
                    (+ x
                       (if (zero? x)
                         0
                         (ask-until-zero ignored)))))])
          (displayln (ask-until-zero #f))))))

;; (add-many-numbers.exe.com)

(define-type ExprC
  [numC (val number?)]
  [unaC (op procedure?) (arg ExprC?)]
  [binC (op procedure?) (lhs ExprC?) (rhs ExprC?)]
  [ifC (cnd ExprC?) (lhs ExprC?) (rhs ExprC?)]
  [idC (id symbol?)]
  [appC (fun ExprC?) (arg ExprC?)]
  [lamC (arg symbol?) (body ExprC?)])

(define (+C l r)
  (binC + l r))
(define (read-numberC a)
  (unaC (λ (cnt return)
          (read-number/and-then
           (format "Enter number ~a:" cnt)
           return))
        a))
(define (letC bi be body)
  (appC (lamC bi body) be))
(define (recC bi be body)
  (letC bi
        (appC
         (lamC 'make-thing
               (letC 'omega
                     (lamC 'x
                           (appC (idC 'make-thing)
                                 (lamC 'v
                                       (appC (appC (idC 'x) (idC 'x)) (idC 'v)))))
                     (appC (idC 'omega) (idC 'omega))))
         (lamC bi be))
        body))

(define-type Value
  [numV (n number?)]
  [closV (f procedure?)])

(define-type Binding
  [bind (name symbol?) (val Value?)])

(define mt-env
  (λ (id)
    (error 'lookup "undefined identifier")))
(define (extend-env new-binding old-env)
  (λ (id)
    (cond
      [(symbol=? id (bind-name new-binding))
       (bind-val new-binding)]
      [else
       (lookup id old-env)])))

(define (lookup id env)
  (env id))

(define (interp p env return)
  (type-case
   ExprC p
   [numC
    (val)
    (return (numV val))]
   [unaC
    (op rhs)
    (interp rhs env
            (λ (rhs-v)
              (op (numV-n rhs-v)
                  (λ (op-v)
                    (return
                     (numV
                      op-v))))))]
   [binC
    (op lhs rhs)
    (interp lhs env
            (λ (lhs-v)
              (interp rhs env
                      (λ (rhs-v)
                        (return
                         (numV
                          (op (numV-n lhs-v)
                              (numV-n rhs-v))))))))]
   [idC
    (id)
    (return (lookup id env))]
   [ifC
    (cnd tru fal)
    (interp cnd env
            (λ (cnd-v)
              (if (zero? (numV-n cnd-v))
                (interp tru env return)
                (interp fal env return))))]
   [lamC
    (arg body)
    (return
     (closV
      (λ (arg-value dyn-return)
        (interp body
                (extend-env
                 (bind arg arg-value)
                 env)
                dyn-return))))]
   [appC
    (fun arg)
    (interp fun env
            (λ (fun-v)
              (interp arg env
                      (λ (arg-v)
                        ((closV-f fun-v)
                         arg-v
                         return)))))]))

(define (interp* c)
  (interp c mt-env (λ (x) x)))

(define (add-two-numbers.exeC)
  (interp*
   (+C
    (read-numberC (numC 1))
    (read-numberC (numC 2)))))

(define (add-many-numbers.exeC)
  (interp*
   (recC
    'ask-until-zero
    (lamC 'count
          (letC 'x
                (read-numberC (idC 'count))
                (+C
                 (idC 'x)
                 (ifC (idC 'x)
                      (numC 0)
                      (appC (idC 'ask-until-zero)
                            (+C (numC 1) (idC 'count)))))))
    (appC (idC 'ask-until-zero)
          (numC 0)))))

;; (add-many-numbers.exeC)

;; int f () {
;;  int x = 0;
;;  while ( true ) {
;;   yield x++;
;;  }
;; }

;; printf f() // ---> 0
;; printf f() // ---> 1

;; (run
;;  (cpser
;;   (let1 ([f
;;           (λ
;;            (ignored)
;;            (let1 ([x 0])
;;                  (rec1 ([while
;;                             (λ (ignored)
;;                               (begin (return (set! x (+ x 1)))
;;                                      (while 0)))])
;;                        (while 0))))])
;;         (begin (displayln (f 0))
;;                (displayln (f 0))))))

(let ([x 1])
  (+ x x))

;; (let ([the-name the-thing])
;;   the-place-to-use-the-name)

;; (let ([x the-continuation-right-now])
;;   (+ x x))

;; let/cc = let current continuation be X
(let/cc x
  (displayln (λ (+) +)))

(define-syntax-rule (while cnd body ...)
  (letrec ([while-loop
            (λ ()
              (when cnd
                body
                ...
                (while-loop)))])
    (while-loop)))

"expanded"
(let ([f (λ ()
           (let/cc return
             (define x 0)
             (while true
               (return x)
               (set! x (add1 x)))))])
  (displayln (f))
  (displayln (f)))

(define-syntax-rule
  (λ/return (arg ...) return body ...)
  (λ (arg ...)
    (let/cc return body ...)))

"λ/return"
(let ([f (λ/return
          () return
          (define x 0)
          (while true
            (return x)
            (set! x (add1 x))))])
  (displayln (f))
  (displayln (f)))

(define-syntax-rule
  (λ/yield (arg ...) yield body ...)
  (let ([memory #f])
    (λ (arg ...)
      (if memory
        (let/cc second-return
          (memory second-return))
        (let/cc return
          (let
              ([yield
                (λ (some-value)
                  (set!
                   return
                   (let/cc where-i-was-when-i-called-yield
                     (set! memory
                           where-i-was-when-i-called-yield)
                     (return some-value))))])
            body ...))))))

"λ/yield"
(let ([f (λ/yield
          () return
          (define x 0)
          (while true
            (return x)
            (set! x (add1 x))))])
  (displayln (f))
  (displayln (f)))
