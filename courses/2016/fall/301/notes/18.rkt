#lang racket/base
(require racket/list)

;;;;;;

(define-syntax-rule
  (question prompt
            [option next]
            ...)
  (λ ()
    (printf "~a\n" prompt)
    (for ([o (in-list '(option ...))]
          [i (in-naturals 1)])
      (printf "\t~a. ~a\n" i o))

    (define ans (read))
    (define next-q
      (for/or ([o (in-list (list 'option ...))]
               [n (in-list (list next ...))]
               [i (in-naturals 1)]
               #:when
               (cond
                 [(number? ans)
                  (equal? i ans)]
                 [(string? ans)
                  (equal? o 'other)]))
        n))

    (define rest-of-the-answers (next-q))
    (λ ()
      (printf "~a\n" ans)
      (rest-of-the-answers))))

;; run-survey : QUESTION -> void
(define (run-survey q) ((q)))
(define (end) (λ () (void)))

;;;;;;

(define q1
  (question "What is the best controller for shooters?"
            ["Arcade Stick" q2]
            ["Gamepad" q3]
            ["Keyboard" q3]))
(define q2
  (question "What is the best gate?"
            ["Circular" q3]
            ["Octangonal" q3]
            ["Square" q3]))
(define q3
  (question "Do you prefer horizontal or vertical scrolling?"
            ["Horizontal" q4]
            ["Vertical" q5]))
(define q4
  (question "What is the best horizontal shooter?"
            ["Gradius V" end]
            ["Sexy Parodius" end]
            ["Border Down" end]
            ["Deathsmiles" end]
            [other end]))
(define q5
  (question "Which is the better developer?"
            ["Cave" q6]
            ["Treasure" q7]))
(define q6
  (question "What is Cave's best release?"
            ["Dodonpachi" end]
            ["Ketsui kizuna jigoku tachi" end]
            ["Mushihimesama Futari 1.5" end]
            [other end]))
(define q7
  (question "What is the Treasure's best release?"
            ["Radiant Silvergun" end]
            ["Sin and Punishment" end]
            ["Ikaruga" end]
            ["Sin and Punishment: Star Successor" end]
            [other end]))

(module+ test-gsurvey
  (require racket/port)
  (with-input-from-string
    "1 3 2 1 3"
    (λ () (run-survey q1)))

  (with-input-from-string
    "1 3 2 1 \"Muchi Muchi Pork\""
    (λ () (run-survey q1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(let/cc id expr)
;; _Let_ the _C_urrent _C_ontinuation
;; of _expr_ be named _id_

#;
(define (get-a-number prompt)
  (let/cc then
    (get-a-number&then prompt then)))

(module+ test
  (+ 4 5) "sb" 9 "\n"

  (+ 4 (let/cc then 5))
  "sb" 9 "\n"

  (+ 4 (let/cc then (then 5)))
  ;; "sb" 13 "\n"
  "sb" 9 "\n"

  (+ 4 (let/cc then (+ 10 (then 5))))
  "sb" 9 "\n"

  (+ 4 (let/cc then (+ 10 (then 5))))
  "sb" 9 "\n"

  #;
  (let ([then (λ (ans)
                (displayln (+ 4 ans))
                (displayln "sb")
                (displayln 9)
                (displayln "\n")
                (exit))])
    (+ 10 (then 5)))

  (+ 4 (let/cc then (then (+ 10 5))))
  "sb" 19 "\n"

  (define (f x)
    (let/cc return
      (when (= x 0)
        (return #f))
      (+ 1 (expt (/ 1 x) 2))))

  (f 2) 5/4
  (f 0) #f

  ;; continuations are control-points

  ;; let/cc names control-points

  #|
  for ( ... ) {
  for ( ... ) {
  break to outer loop;
  }
  }
  |#

  (define throw-up-stack empty)
  (define-syntax-rule
    (try tried-expr #:catch catch-expr)
    (let ([ans
           (let/cc here
             (set! throw-up-stack
                   (cons (λ () (here catch-expr))
                         throw-up-stack))
             tried-expr)])
      (set! throw-up-stack (rest throw-up-stack))
      ans))
  (define (throw)
    (if (empty? throw-up-stack)
      (error 'throw "No catch handler")
      ((first throw-up-stack))))

  (define (safe/ x y)
    (when (= y 0)
      (throw))
    (/ x y))

  (+ 5
     (try
      (* 4 (safe/ 1 0))
      #:catch
      (begin (printf "Caught it!\n")
             5)))

  (define (g y)
    (let/cc return
      (map (λ (n)
             (when (= n 0)
               (return #f))
             (/ 1 n))
           y)))
  (g (list 5 6 7 3 0 5 7))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (thread1)
    (for ([i (in-range 20)])
      (os-printf "ping!\n"))
    (os-exit))
  (define (thread2)
    (for ([i (in-range 20)])
      (os-printf "pong!\n"))
    (os-exit))

  (define THREADS empty)
  (define (os-exit)
    (call-next-thread))
  (define (call-next-thread)
    (define next-thread (first THREADS))
    (set! THREADS (rest THREADS))
    (next-thread))
  (define (yield)
    (let/cc current-thread-state
      (set! THREADS
            (append THREADS
                    (list current-thread-state)))
      (call-next-thread)))
  (define (kernel list-of-threads)
    (set! THREADS list-of-threads)
    (define (loop)
      (unless (empty? list-of-threads)
        (yield)
        (loop)))
    (loop))
  (define (os-printf str)
    (printf str)
    (yield))

  #;(kernel (list thread1 thread2))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (one-of opts)
    (cond
      [(empty? opts)
       (fail)]
      [else
       (let ([old-fail fail])
         (let/cc go-back
           (set! fail
                 (λ ()
                   (set! fail old-fail)
                   (go-back
                    (one-of (rest opts)))))
           (first opts)))]))
  (define fail
    (λ ()
      (error 'fail "No valid options")))

  (let ([a (one-of (list 4 5 6))]
        [b (one-of (list 8 9 10))])
    (printf "TRY a is ~a, b is ~a\n"
            a b)
    (unless (= (+ a b) 15)
      (fail))
    (printf "OK a is ~a, b is ~a\n"
            a b))

  )
