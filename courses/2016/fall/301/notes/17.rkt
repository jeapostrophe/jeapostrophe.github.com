#lang racket/base
(require racket/list)

(define-syntax-rule
  (dfa start-state (accept-state ...)
       [state (symbol -> next-state)
              ...]
       ...)
  (λ (some-list)
    (define (accepting? some-state)
      (or (eq? some-state accept-state)
          ...))
    ;; state : list-of-symbols -> true-or-false
    (define (state some-list)
      (if (empty? some-list)
        (accepting? state)
        (case (first some-list)
          [(symbol)
           (next-state (rest some-list))]
          ...)))
    ...
    (start-state some-list)))

(define odd-and-0-OR-even-and-1?
  (dfa start (odd-and-0 even-and-1)
       [start (0 -> odd-and-0)
              (1 -> odd-and-1)]
       [odd-and-0
        (0 -> even-and-0)
        (1 -> even-and-0)]
       [even-and-0
        (0 -> odd-and-0)
        (1 -> odd-and-0)]
       [odd-and-1
        (0 -> even-and-1)
        (1 -> even-and-1)]
       [even-and-1
        (0 -> odd-and-1)
        (1 -> odd-and-1)]))

(odd-and-0-OR-even-and-1?
 '(1 0 0)) "should be" #f

(odd-and-0-OR-even-and-1?
 '(0 1 0)) "should be" #t










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; add-two-numbers.com
;;  Enter first number: <...>
;; Enter second number: <...>
;; Your sum is: <....>

;; add-two-numbers.exe
(define (get-a-number prompt)
  (printf "<html><body>Enter ~a number:</body></html>\n"
          prompt)
  (read))
(define (add-two-numbers.exe)
  (displayln
   (+ (get-a-number "first")
      (get-a-number "second"))))

;; (add-two-numbers.exe)


(define first-number-db #f)
(define second-number-db #f)
(define (get-a-number&then-database prompt then)
  (printf "<html><body>Enter ~a number:</body></html>\n"
          prompt)
  (define v (read))
  (if first-number-db
    (set! second-number-db v)
    (set! first-number-db v))
  (then))

(define (get-a-number&then-hacked
         prompt other-stuff then)
  (printf "<html><body><hidden>~a</hidden>Enter ~a number:</body></html>\n"
          other-stuff
          prompt)
  (define v (read))
  (then other-stuff v))

(define (add-two-numbers.com/index-hack)
  (get-a-number&then
   "first"
   #f
   add-two-numbers.com/get2))

(define (add-two-numbers.com/get2-hack ignored first-number)
  (get-a-number&then
   "second"
   first-number
   add-two-numbers.com/display-hack))

(define (add-two-numbers.com/display-hack
         first-number second-number)
  (displayln (+ first-number
                second-number)))

;; (add-two-numbers.com/index)

;; new

(define database (make-hasheq))
(define (server url val)
  (printf "Navigating to ~a with ~a\n" url val)
  ((hash-ref database url) val))
(define counter 0)
(define (new-url)
  (set! counter (add1 counter))
  counter)

(define (get-a-number&then
         prompt then)
  (define u (new-url))
  (printf "u = ~a\n<html><body>Enter ~a number:</body></html>\n" u
          prompt)
  (hash-set! database u then))

(define (add-two-numbers.com/index)
  (get-a-number&then
   "first"
   add-two-numbers.com/get2))

(define (add-two-numbers.com/get2 first-number)
  (define (add-two-numbers.com/display?first-number=xxx
           second-number)
    (displayln (+ first-number
                  second-number)))
  (get-a-number&then
   "second"
   add-two-numbers.com/display?first-number=xxx))
;;

(define (chrome home-page)
  (printf "What url?\n")
  (define u (read))
  (cond
    [u
     (printf "What value?\n")
     (define v (read))
     (printf "Read ~v\n" v)
     (server u v)]
    [else
     (printf "Running home-page\n")
     (home-page)])
  (chrome home-page))

;; (chrome add-two-numbers.com/index)

;;;;

(define-syntax webify
  (syntax-rules (λ displayln + get-a-number)
    [(_ (λ () body))
     (λ () (webify body))]
    [(_ (displayln e))
     ((webify e)
      (λ (ans)
        (displayln ans)))]
    [(_ (+ lhs rhs))
     (λ (go-back)
       ((webify lhs)
        (λ (lhs-v)
          ((webify rhs)
           (λ (rhs-v)
             (go-back (+ lhs-v rhs-v)))))))]
    [(_ (get-a-number prompt))
     (λ (then) (get-a-number&then prompt then))]))

(chrome
 (webify
  (λ ()
    (displayln
     (+ (get-a-number "first")
        (get-a-number "second"))))))

;; webify is REALLY called CPS
;; CPS stands for "continuation-passing style"

;; a stack IMPLEMENTS a continuation

