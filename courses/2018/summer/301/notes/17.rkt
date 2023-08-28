#lang racket/base
(require racket/list racket/match)

;; Basic Examples
(+ 20 22)

(+ 20 (call/cc (λ (k) 22)))

(+ 20 (call/cc (λ (k) (k 22))))

(+ 20 (call/cc (λ (k) (+ 10 (k 22)))))
;; 72?
;; ==> 42

(+ 20 (let/cc k (+ 10 (k 22))))

;; C-style control flow
(define (f x)
  (let/cc return
    (when (< x 0) (return -1))
    (+ (* x x) x)))

(f -1)
(f 10)

(define-syntax-rule (while c b ...)
  (letrec ([f (λ () (when c b ... (f)))])
    (f)))

(define (g x)
  (let/cc break
    (while (< x 10)
      (let/cc continue
        (set! x (* x x))
        (when (= x 4) (continue))
        (when (even? x)
          (break)))))
  x)

(g 2)

;; ISWIM
;; (begin stmt1 (let/cc k stmt2 (k 2)) stmt3 stmt4)

;; C:
;;  stmt1
;;  stmt2
;;  goto k;
;; k:
;;  stmt3
;;  stmt4

;; Exception Handling
(define default-handler
  (λ (x)
    (eprintf "~a\n" x)
    (exit -1)))
(define current-handler (box default-handler))
(define (my-error msg)
  ((unbox current-handler) msg))
(define (my-handle try catch)
  (define old-handler (unbox current-handler))
  (begin0
      (let/cc caller-of-my-handle
        (set-box! current-handler
                  (λ (x) (caller-of-my-handle (catch x))))
        (try))
    (set-box! current-handler old-handler)))

(define (my-div x y)
  (when (zero? y) (my-error "Can't divide by zero"))
  (/ x y))
(for ([i (in-range 10 -1 -1)])
  (printf "1 / ~a = ~a\n" i
          (my-handle (λ () (my-div 1 i))
                     (λ (x) "undefined"))))

;; Multi-threading (i.e. concurrency)
;; (not necessarily parallelism)

(define (snoc l x) (append l (list x)))
(define THREADS (box empty))
(define DONE-K (box #f))
(define (add-to-THREADS t)
  (set-box! THREADS (snoc (unbox THREADS) t)))
(define (initialize t)
  (let/cc whole-system
    (set-box! DONE-K whole-system)
    (add-to-THREADS t)
    (stop 'initialize)))
(define (spawn t)
  (add-to-THREADS t)
  (yield))
(define (yield)
  (let/cc me
    (add-to-THREADS me)
    (stop 'yield)))
(define (stop which)
  (printf "STOP @ ~a w/ ~a\n" which (unbox THREADS))
  (match (unbox THREADS)
    ['() ((unbox DONE-K))]
    [(cons next-t other-threads)
     (set-box! THREADS other-threads)
     (next-t)]))

(define (thread-1)
  (spawn thread-2)
  (for ([i (in-range 10)])
    (printf "1. ~a\n" i)
    (yield))
  (stop 'thread-1))
(define (thread-2)
  (for ([i (in-range 10 25)])
    (printf "2. ~a\n" i)
    (yield))
  (stop 'thread-2))

(initialize thread-1)

;;;;;;;;;;;;;;;;;;;;;;;;;

;; Angelic Computation
;; aka Non-determinism
;; aka Backtracking Search

(define try-again
  (box (λ () (error 'fail "No more options"))))
(define (fail)
  ((unbox try-again)))
(define (ensure test)
  (unless test
    (fail)))
(define (in options)
  (when (empty? options)
    (fail))
  (match-define (cons opt1 more-options) options)
  (let/cc ins-caller
    (define old-try-again (unbox try-again))
    (set-box! try-again
              (λ ()
                (set-box! try-again old-try-again)
                (ins-caller (in more-options))))
    opt1))

(let ([x (in '(4 2 1 3))]
      [y (in '(6 7 8 9))])
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
                    (let/cc next-ans
                      (set! f-in-progress next-ans)
                      (current-return ans)))))
         (current-return #f)]))))

(define route
  (make-generator
   (λ (return)
     (return "Lowell")
     (return "Chelmsford")
     (return "Westford")
     (return "Tyngsboro")
     (return "Dunstable")
     (return "Nashua"))))

(list (route) (route) (route)
      (route) (route) (route)
      (route) (route))

(define evens
  (make-generator
   (λ (return)
     (for ([i (in-naturals)])
       (return (* i 2))))))
(list (evens) (evens) (evens) (evens)
      (evens) (evens) (evens) (evens))

;;;;;;;;;;;;;;;;;;;;

;; Web Programming

(define stop! (box #f))
(define (web-display msg)
  ((unbox stop!) msg))
(define (web-ask msg where-to-next)
  (web-display (list where-to-next msg)))
(define (web-read)
  (read))
(define (browse url ans)
  (match-define (list where-to-next msg)
    (let/cc this-stop
      (set-box! stop! this-stop)
      (url ans)))
  (displayln msg)
  where-to-next)

(define (ask-for-a-number which)
  (web-display (format "Give me a ~a number" which))
  (web-read))
(define (add-two-numbers.com)
  (web-display (+ (ask-for-a-number "first")
                  (ask-for-a-number "second"))))

(define (ask-for-a-number-by-node.js which what-next)
  (web-ask (format "Give me a ~a number" which) what-next))
(define (add-two-numbers.com-by-node.js ignored)
  (ask-for-a-number-by-node.js
   "first"
   (λ (first-num)
     (ask-for-a-number-by-node.js
      "second"
      (λ (second-num)
        (web-ask (+ first-num second-num) void))))))

(define add-two-numbers.com-by-node.js/give-first
  (browse add-two-numbers.com-by-node.js #f))
(define add-two-numbers.com-by-node.js/give-second
  (browse add-two-numbers.com-by-node.js/give-first 5))
(browse add-two-numbers.com-by-node.js/give-second 37)
(browse add-two-numbers.com-by-node.js/give-second 30)
(browse add-two-numbers.com-by-node.js/give-second 41)

(define (web-awesome-ask msg)
  (let/cc what-next
    (web-ask msg what-next)))

(define (ask-for-a-number-awesome which)
  (web-awesome-ask (format "Give me a ~a number" which)))
(define (add-two-numbers.com-awesome ignored)
  (web-ask (+ (ask-for-a-number-awesome "first")
              (ask-for-a-number-awesome "second")) void))

(define add-two-numbers.com-awesome/give-first
  (browse add-two-numbers.com-awesome #f))
(define add-two-numbers.com-awesome/give-second
  (browse add-two-numbers.com-awesome/give-first 5))
(browse add-two-numbers.com-awesome/give-second 37)
(browse add-two-numbers.com-awesome/give-second 30)
(browse add-two-numbers.com-awesome/give-second 41)
