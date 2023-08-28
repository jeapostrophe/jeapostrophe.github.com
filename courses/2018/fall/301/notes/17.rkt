#lang racket/base
(require racket/list racket/match)

;; Simple
(+ 20 22)

;; k = λx.(finish-the-program-with (+ 20 x))
(+ 20 (call/cc (λ (k) 22)))

;; 62 = (+ 20
;;       (finish-the-program-with (+ 20 22)))
(+ 20 (call/cc (λ (k) (k 22))))

(+ 20 (call/cc (λ (k) (+ 10 (k 22)))))

;; (let/cc x m) := (call/cc (λ (x) m))
(+ 20 (let/cc k (+ 10 (k 22))))

;; C-style control flow
(define (f x)
  (let/ec return
    (when (< x 0) (return -1))
    (+ (* x x) x)))

(f 10)
(f -1)

;; int x = 2;
;; while ( x < 10 ) {
;;   x = x * x;
;;   if ( x == 4 ) continue;
;;   if ( x % 2 == 0 ) break;
;; }

(define-syntax-rule (while c b ...)
  (letrec ([f (λ () (when c b ... (f)))])
    (f)))

(define (g x)
  (let/ec return
    (let/ec break
      (while (< x 10)
        (let/ec continue
          (set! x (* x x))
          (when (= x 4) (continue))
          (when (even? x)
            (break)))))
    x))

(g 2)

;; Exception Handler

(define current-handler
  (λ (x)
    (eprintf "error: ~a\n" x)
    (exit 1)))
(define (my-catch code-to-try handler)
  (define old-handler current-handler)
  (begin0
      (let/cc caller
        (set! current-handler
              (λ (x) (caller
                      (handler x))))
        (code-to-try))
    (set! current-handler old-handler)))
(define (my-error msg)
  (current-handler msg))

(define (div x y)
  (when (zero? y)
    (my-error "can't divide by zero"))
  (/ x y))

(my-catch
 (λ ()
   (for ([i (in-range 10 -1 -1)])
     (printf "1 / ~a = ~a\n"
             i
             (my-catch (λ () (div 1 i))
                       (λ (x)
                         (printf "I got called!\n")
                         "undefined")))))
 (λ (x)
   (printf "Couldn't for loop\n")))

;; Multi-threading (or concurrency)

(define (snoc l x) (append l (list x)))
(define the-threads (box empty))
(define THE-END (box #f))
(define (add-to-threads t)
  (set-box! the-threads
            (snoc (unbox the-threads) t)))
(define (initialize t)
  (let/cc very-end
    (set-box! THE-END very-end)
    (add-to-threads t)
    (stop "Initialize")))
(define (spawn t)
  (add-to-threads t)
  (yield))
(define (stop v)
  (match (unbox the-threads)
    ['() ((unbox THE-END) "Totally done")]
    [(cons next-t other-threads)
     (set-box! the-threads other-threads)
     (next-t)]))
(define (yield)
  (let/cc me
    (add-to-threads me)
    (stop "Yield")))

(define (thread-1)
  (spawn thread-2)
  (for ([i (in-range 10)])
    (printf "1. ~a\n" i)
    (yield))
  (stop "Thread 1 done"))
(define (thread-2)
  (for ([i (in-range 10 25)])
    (printf "2. ~a\n" i)
    (yield))
  (stop "Thread 2 done"))

(initialize thread-1)

;; Angelic Computation (semantics)
;; - Back-tracking (implementation strategy)
;; - Constraint solving (impl strat)

(define try-again
  (box (λ () (error 'fail "No possible solution"))))
(define (fail)
  ((unbox try-again)))
(define (ensure test)
  (unless test
    (fail)))
(define (either options)
  (when (empty? options)
    (fail))
  (match-define (cons opt1 more-options) options)
  (let/cc either-caller
    (define old-try-again (unbox try-again))
    (set-box! try-again
              (λ ()
                (set-box! try-again old-try-again)
                (either-caller
                 (either more-options))))
    opt1))

(let ([x (either '(4 2 1 3))]
      [y (either '(6 7 8 9))])
  (printf "TRYING ~a and ~a\n" x y)
  (ensure (= (+ x y) 9))
  (printf "~a + ~a = 9\n" x y))

;; Generators
(define (make-generator f)
  (define f-in-progress #f)
  (λ ()
    (let/cc return
      (cond
        [f-in-progress
         (f-in-progress return)]
        [else
         (define current-return return)
         (f (λ (ans)
              (set! current-return
                    (let/cc next-call
                      (set! f-in-progress next-call)
                      (current-return ans)))))
         (current-return #f)]))))

(define route
  (make-generator
   (λ (return)
     (return "Lowell")
     (return "Chelmsford")
     (return "Westford")
     (return "Tyngsboro")
     (return "Dunstable"))))
(list (route) (route)
      (route) (route)
      (route) (route) (route))

(define evens
  (make-generator
   (λ (return)
     (for ([i (in-naturals)])
       (return (* i 2))))))
(list (evens) (evens) (evens) (evens))

;; Web Programs
