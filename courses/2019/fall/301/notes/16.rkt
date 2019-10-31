#lang racket/base
(require racket/list)

(define (check x y)
  (unless (equal? x y)
    (eprintf "~a should be ~a\n" x y)))

(check (+ 1 (call/cc (λ (k) (+ 2 (k 3)))))
       ;; 7, 6, 4
       4)

(check (+ 1 (call/cc (λ (k) (* 2 (- 3 (+ 2 (k 3)))))))
       4)

;; (let/cc k e ...) => (call/cc (λ (k) e ...))

(check (+ 1 (let/cc k
              (* 2 (- 3 (+ 2 (k 3))))))
       4)

(define (f x)
  (let/cc return
    (when (zero? x)
      (return 0))
    (/ 2 x)))
(check (f 2) 1)
(check (f 4) 1/2)
(check (f 0) 0)

(define (fp x)
  (let/cc return
    (let ([ignored (when (zero? x)
                     (return 0))])
      (/ 2 x))))
(check (fp 2) 1)
(check (fp 4) 1/2)
(check (fp 0) 0)

(define (g helper x)
  (define y 0)
  (define (the-while-fun)
    (let/cc break
      (when (< y x)
        (let/cc continue
          ;; y = helper(break, y)
          (set! y (helper break y))
          (when (even? y)
            (continue))
          (printf "y = ~a\n" y))
        (the-while-fun))))
  (the-while-fun)
  (- x y))
(check (g (λ (break y)
            (when (= y 4)
              (break))
            (add1 y))
          5) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (snoc l x) (append l (list x)))

(define Q '())
(define (spawn! thread-body)
  (set! Q (snoc Q thread-body)))
(define (yield!)
  (let/cc me
    (spawn! me)
    (exit!)))
(define (exit!)
  (cond
    [(empty? Q)
     (printf "I'm done!\n")
     (exit 0)]
    [else
     (define next (first Q))
     (set! Q (rest Q))
     (next)]))

;; A channel is either
;;   - never been written      ---- #f
;;   - someone wrote something ---- ?????
;;   - someone is trying read  ---- ?????
(struct someone-wrote (v who before))
(struct someone-is-reading (who who-else-is-reading))
(define (make-chnl)
  (box #f))
(define (chnl-read ch)
  (define chc (unbox ch))
  (cond
    ;; If someone already wrote
    ;; ....return it
    [(someone-wrote? chc)
     (set-box! ch (someone-wrote-before chc))
     (spawn! (someone-wrote-who chc))
     (someone-wrote-v chc)]
    [else
     ;; Add ourself as a reader
     (let/cc me
       (set-box! ch (someone-is-reading me chc))
       (exit!))]))
(define (chnl-write ch v)
  (define chc (unbox ch))
  (cond
    [(someone-is-reading? chc)
     (spawn! (λ () ((someone-is-reading-who chc) v)))
     (set-box! ch (someone-is-reading-who-else-is-reading chc))]
    [else
     (let/cc me
       (set-box! ch (someone-wrote v me chc))
       (exit!))]))

;; make-sema :: (box (cons 0 '()))
;; sema-wait :: if the number is 0, add my continuation to queue
;;              o.w., decrement the number
;; sema-post :: if the list is empty, add 1 to the number
;;              o.w., activate the first thing in the queue (and pop it)

#;(let ()
    (define channel42 (make-chnl))
    (spawn! (λ ()
              (for ([i (in-range 3)])
                (printf "A says ~a\n" i)
                (yield!))
              (printf "A is trying to read\n")
              (define the-place (chnl-read channel42))
              (printf "A thinks the-place is ~a\n" the-place)
              (exit!)))
    (spawn! (λ ()
              (for ([i (in-range 3)])
                (printf "B says ~a\n" i)
                (yield!))
              (printf "B is trying to write\n")
              (chnl-write channel42 5)
              (printf "B wrote\n")
              (exit!)))
    (exit!))

(define try-again
  (box
   (λ ()
     (error 'try-again "Tried to try again, but had no options!"))))
(define (either opts)
  (let/cc here
    (when (empty? opts)
      (fail!))
    (define old-try-again (unbox try-again))
    (set-box! try-again
              (λ ()
                (set-box! try-again old-try-again)
                (here (either (rest opts)))))
    (first opts)))
(define (fail!)
  ((unbox try-again)))

(define (assert! condition)
  (unless condition
    (fail!)))

(let ()
  (define A (either (list 1 2 3)))
  (define B (either (list 4 5 6)))
  (printf "Trying... A=~a, B=~a\n" A B)
  (assert! (= (+ A B) 7))
  (assert! (> A 1))
  (printf "A = ~a, B = ~a\n" A B))
