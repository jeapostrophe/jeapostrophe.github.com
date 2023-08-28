#lang plai

;; YESTERDAY
;; - Web style is called CPS---continuation passing style
;; - A continuation is the abstraction that a stack implements
;; -- "The rest of the computation"
;; - CPS can be automated for every program in every programming language
;; -- But is easier with closures
;; -- When you CPS, some functions (like +) are oblivious to the continuation
;; --- But others (like web-prompt/clo) observe it

;; TODAY
;; - What other functions could we write that observe the continuation?
;; - How else could first-class continuation access be available?
;; -- CPS is bad because...
;; --- global,
;; --- doesn't use stack,
;; --- requires proper function call implementations

;; - Examples              [0:10]
;; -- call/cc
;; -- let/cc               [0:15]
;; -- escaping
;; -- exceptions           [0:20]
;; -- threads              [0:30]
;; -- generators           [0:50]

;; !!! We did not get to the very end of generators (the infinite loop fix)

;; - More (preview)
;; -- backtracking
;; -- delimited
;; -- continuation marks   [0:50.99]

;; First-class continuations

(+ 1 (+ 2 3))

;; The continuation of (+ 2 3) is...
(λ (x)
  (displayln (+ 1 x))
  (exit 0))

;; The continuation of (+ 1 (+ 2 3)) is...
(λ (x)
  (displayln x)
  (exit 0))

(+ 1
   (call-with-current-continuation
    (λ (k)
      (+ 2 3))))

(+ 1
   (call/cc
    (λ (k)
      (+ 2 3))))

(+ 1
   (let/cc k
     (+ 2 3)))

(+ 1
   (let/cc k
     (+ 2 (k 3))))
;; infinite loop
;; 7
;; 8 (il but stops once)
;; 4... 0

(+ 2
   (+ 1
      (let/cc k
        (+ 2 (k 3)))))

;; Error
;; (+ 2
;;    (+ 1
;;       (let/cc k
;;         (+ 2 (+ k 3)))))

(define (f x)
  (let/cc return
    (when (> x 5)
      (return -1))
    (+ x 5)))

(f 5)
(f 7)

;; (try body (catch (λ (exn) ...)))
;; ->
(define last-exn-handler #f)
(define (try body-f catch-f)
  ;; XXX no stack
  (let/cc before-body-f-k
    (set! last-exn-handler
          (λ (x)
            (before-body-f-k
             (catch-f x))))
    (body-f)))
(define (throw val)
  (last-exn-handler val))

(define (h y)
  (when (zero? y)
    (throw "can't divide by zero"))
  (/ 10 y))
(define (g x)
  (try
   (λ ()
     (h (- x 5)))
   (λ (x)
     (eprintf "~a\n" x)
     10)))

(g 10)
(g 5)

;; threading system
(define thread-q empty)
(define (spawn f)
  (set! thread-q
        (append thread-q
                (list f))))
(define (yield)
  (let/cc when-i-come-back-do-this
    (spawn when-i-come-back-do-this)
    (define next-guy (first thread-q))
    (set! thread-q (rest thread-q))
    (next-guy)))

(spawn
 (λ ()
   (printf "I am running\n")
   (for ([i (in-range 10)])
     (printf "~a\n" i)
     (yield))))
(printf "He is running\n")
(for ([i (in-range 10 20)])
  (printf "other guy ~a\n" i)
  (yield))

;; generator

(define (make-generator generator-body)
  (define where-i-was #f)
  (define (whered-theyd-go)
    (if where-i-was
      (where-i-was)
      (let/cc the-real-return
        (define (return answer)
          (let/cc where-i-am
            (set! where-i-was
                  where-i-am)
            (the-real-return
             answer)))
        (generator-body return)
        #f)))
  whered-theyd-go)

;; (define where-i-was #f)
;; (define (whered-theyd-go)
;;   (if where-i-was
;;     (where-i-was)
;;     (let/cc the-real-return
;;       (define (return answer)
;;         (let/cc where-i-am
;;           (set! where-i-was
;;                 where-i-am)
;;           (the-real-return
;;            answer)))
;;       ;; where-i-am goes here --->
;;       (return "Kirtland")
;;       ;; <----
;;       (return "Far West")
;;       (return "Nauvoo")
;;       (return "SLC")
;;       ;; the last where-i-was goes here --->
;;       (return "Celestial Kingdom (Provo)")
;;       ;; <----
;;       #f)))

(define whered-theyd-go
  (make-generator
   (λ (return)
     (return "Kirtland")
     (return "Far West")
     (return "Nauvoo")
     (return "SLC")
     (return "Celestial Kingdom (Provo)")))  )

;; the-real-return goes here-->
;; Infinite loop
;; (list (whered-theyd-go)
;;       (whered-theyd-go))
(whered-theyd-go)
;; <---
(whered-theyd-go)
(whered-theyd-go)
(whered-theyd-go)
(whered-theyd-go)
(whered-theyd-go)
(whered-theyd-go)

;;(set! where-i-was #f)
;;(whered-theyd-go)

(define what-are-the-odds?
  (make-generator
   (λ (return)
     (define (loop i)
       (return i)
       (loop (+ i 2)))
     (loop 1))))

(what-are-the-odds?)
(what-are-the-odds?)
(what-are-the-odds?)

"end"
