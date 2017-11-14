#lang racket/base
(require racket/list)

;; Rule 1
;; E [ call/cc V ] --> E [ V (λ (x) E[x]) ]

;; Rule 2
;; E [ call/cc V ] --> E [ V (kont E) ]
;; E [ (kont E') V ] --> E' [ V ]

;;;; Simple Examples
(call/cc (λ (k) (k 12))) ;; => 12

;; k = (λ (x) (+ 3 x))
(+ 3 (call/cc (λ (k) (k 12)))) ;; => 15

(+ 3 (let/cc k (k 12))) ;; => 15

(+ 3 (let/cc k (+ 10 (k 12)))) ;; => 15

;;;; Exception Handling
(define the-exception-handler
  (λ (x)
    (printf "An exception was not caught: ~e\n" x)
    (exit 1)))
(define (throw v)
  (the-exception-handler v))
(define (catch body-block handler-fun)
  (let/cc where-i-was
    (define last-handler the-exception-handler)
    (set! the-exception-handler
          (λ (x)
            (where-i-was (handler-fun x))))
    (define answer (body-block))
    (set! the-exception-handler last-handler)
    answer))

(define (my-divide x y)
  (if (zero? y)
    (throw "Division by zero")
    (/ x y)))

#;(let ()
    (my-divide 6 (- 7 7))) ;; => returns exception

(let ()
  (+ 5
     (catch (λ () (+ 6 (my-divide 6 (- 7 7))))
       (λ (x)
         (printf "Caught exception: ~e\n" x)
         0))))
;; => 5

(let ()
  (+ 5
     (catch (λ () (+ 6 (my-divide 6 (- 7 6))))
       (λ (x)
         (printf "Caught exception: ~e\n" x)
         0))))
;; => 17

;;;; Back-tracking

;; one-of : list of stuff -> one of the stuff
;; Returns the stuff so that the program doesn't fail
#;(let ()
    (define how-many-times-called 0)
    (define (one-of l)
      (set! how-many-times-called
            (add1 how-many-times-called))
      (cond
        [(= how-many-times-called 1) (first l)]
        [(= how-many-times-called 2) (third l)]
        [else (error 'one-of "I don't know")]))
    (define (fail)
      (error 'fail "You failed, fix the implemention of one-of")))

(define (one-of l)
  (cond
    [(empty? l)
     (fail)]
    [else
     (let/cc come-back-here
       (define orig-fail fail)
       (set! fail
             (λ ()
               (set! fail orig-fail)
               (come-back-here (one-of (rest l)))))
       (first l))]))
(define fail
  (λ () (error 'fail "No choice works")))

(let ()
  (define a (one-of (list 4 5 6)))
  (define b (one-of (list 3 7 9)))
  (printf "\tA = ~a, B = ~a\n" a b)
  (when (not (= (+ a b) 13))
    (fail))
  (format "The answer is: ~a ~a" a b))
;; => The answer is: 4 9
;; => The answer is: 6 7

;;;; Generator
(let ()
  (define where-multiples-of-eight-was #f)
  (define (multiples-of-eight)
    (let/cc current-call-site
      (define (return v)
        (set! current-call-site
              (let/cc where-i-was
                (set! where-multiples-of-eight-was where-i-was)
                (current-call-site v))))
      (cond
        [where-multiples-of-eight-was
         (where-multiples-of-eight-was current-call-site)]
        [else
         (for ([i (in-naturals)])
           (return (* i 8)))])))
  (list (multiples-of-eight)
        (multiples-of-eight)
        (multiples-of-eight)
        (multiples-of-eight)
        (multiples-of-eight)))
;; => 0, 8, 16, 24, 32

(define (make-generator work)
  (define where-work-was #f)
  (define (the-generator)
    (let/cc current-call-site
      (define (return v)
        (set! current-call-site
              (let/cc where-i-was
                (set! where-work-was where-i-was)
                (current-call-site v))))
      (cond
        [where-work-was
         (where-work-was current-call-site)]
        [else
         (work return)])))
  the-generator)
(let ()
  (define multiples-of-eight
    (make-generator
     (λ (return)
       (for ([i (in-naturals)])
         (return (* i 8))))))
  (list (multiples-of-eight)
        (multiples-of-eight)
        (multiples-of-eight)
        (multiples-of-eight)
        (multiples-of-eight)))
;; => 0, 8, 16, 24, 32

(let ()
  (define even-numbers-minus-two-gen
    (make-generator
     (λ (return)
       (for ([i (in-naturals)])
         (when (even? i)
           (return (- (* i 2) 2)))))))
  (list (even-numbers-minus-two-gen)
        (even-numbers-minus-two-gen)
        (even-numbers-minus-two-gen)
        (even-numbers-minus-two-gen)
        (even-numbers-minus-two-gen)
        (even-numbers-minus-two-gen)))

;;;; Threads

(define THREADS empty)
(define (make-process thread-body)
  (set! THREADS (append THREADS (list thread-body))))
(define (yield)
  (let/cc remember-where-it-was
    (make-process remember-where-it-was)
    (switch-to-another-thread)))
(define (process-exit)
  (switch-to-another-thread))
(define (switch-to-another-thread)
  (cond
    [(empty? THREADS)
     (original-call-to-the-kernel)]
    [else
     (define next-thread (first THREADS))
     (set! THREADS (rest THREADS))
     (next-thread)]))
(define original-call-to-the-kernel #f)
(define (start-the-kernel!)
  (let/cc my-caller
    (set! original-call-to-the-kernel my-caller)
    (switch-to-another-thread)))

(let ()
  (make-process
   (λ ()
     (for ([i (in-range 10)])
       (printf "Process A looked at ~a\n" i)
       (yield))
     (process-exit)))
  (make-process
   (λ ()
     (for ([i (in-range 30 40)])
       (printf "Process B looked at ~a\n" i)
       (yield))
     (process-exit)))
  (printf "About to start the kernel\n")
  (start-the-kernel!)
  (printf "Kernel is done\n"))

(define (real-printf fmt args)
  ;; 64 means "do a printf"
  (yield 64 fmt args))

;;;; Web Programming
(require web-server/servlet-env
         web-server/servlet
         web-server/http)
;; add-two-numbers.com
(define (get-a-number label)
  (string->number
   (extract-binding/single
    'thenum
    (request-bindings
     (send/suspend
      (λ (new-url-for-here)
        (send/back
         (response/xexpr
          `(body
            (h1 ,(format "What is the ~a number?" label))
            (form ([action ,new-url-for-here])
                  (input ([type "text"] [name "thenum"]))
                  (input ([type "submit"]))))))))))))

(define (add-two-numbers.com)
  (serve/servlet
   (λ (req)
     (define x (get-a-number "First"))
     (define y (get-a-number "Second"))
     (define ans (+ x y))
     (send/back
      (response/xexpr
       `(body
         (h1 ,(format "The answer is ~a" ans))))))))

(define (add-many-numbers.com)
  (serve/servlet
   (λ (req)
     (define how-many (get-a-number "How Many"))
     (define ans
       (for/sum ([i (in-range how-many)])
         (get-a-number (number->string i))))
     (send/back
      (response/xexpr
       `(body
         (h1 ,(format "The answer is ~a" ans))))))))

(add-many-numbers.com)
