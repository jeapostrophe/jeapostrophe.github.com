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

(run
 (cpser
  (rec1 ([ask-until-zero
          (λ (ignored)
            (let1 ([x (read-number "Gimme another")])
              (+ x
                 (if (zero? x)
                   0
                   (ask-until-zero ignored)))))])
        (displayln (ask-until-zero #f)))))
