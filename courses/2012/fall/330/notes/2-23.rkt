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

(+ 1 (+ 2 3))

;; What is the continuation of "(+ 1 (+ 2 3))"?
(λ (x)
  (displayln x)
  (exit 0))

;; What is the continuation of "(+ 2 3)"?
(λ (five)
  (displayln (+ 1 five))
  (exit 0))

(+ 1
   (call-with-current-continuation
    (λ (the-continuation-of-+-2-3)
      (+ 2 3))))

(+ 1
   (call/cc
    (λ (the-continuation-of-+-2-3)
      (+ 2 3))))

(+ 1
   (let/cc the-continuation-of-+-2-3
     (+ 2 3)))

(+ 1
   (call-with-current-continuation
    (λ (the-continuation-of-+-2-3)
      (+ 2 (the-continuation-of-+-2-3 3)))))

(+ 1
   (let/cc the-continuation-of-+-2-3
     (+ 2 (the-continuation-of-+-2-3 3))))

;;;;;
(define (f x)
  (let/cc return
    (when (> x 5)
      (return -1))
    (+ x 2)))

(f 0)
(f 7)

;;;;;
(define the-exn-handler #f)
(define (throw x)
  (the-exn-handler x))
(define (try-catch body-f catch-f)
  (define old-exn-handler the-exn-handler)
  (begin0
    (let/cc who-called-try-catch
      (set! the-exn-handler
            (λ (x)
              (who-called-try-catch
               (catch-f x))))
      (body-f))
    (set! the-exn-handler
          old-exn-handler)))

(define (h x)
  (when (zero? x)
    (throw "You can't do that!"))
  (/ 10 x))
(define (g x)
  (try-catch
   (λ () (h (- x 5)))
   (λ (x)
     (eprintf "~a\n" x)
     -1)))

(g 10) "should be 2"
(g 5) "should be -1 + error"

;; threading systems

(define thread-q empty)
(define (spawn thread-f)
  (set! thread-q
        (append thread-q
                (list thread-f))))
(define (context-switch)
  (let/cc where-i-am-now-so-i-can-come-back-later
    (spawn where-i-am-now-so-i-can-come-back-later)
    (define whose-next (first thread-q))
    (set! thread-q (rest thread-q))
    (whose-next)))

(spawn
 (λ ()
   (for ([i (in-range 10)])
     (printf "him: ~a\n" i)
     (context-switch))))
(for ([i (in-range 10)])
  (printf "me: ~a\n" i)
  (context-switch))

;; generator

;; (define where-i-was #f)
;; (define (whered-they-go)
;;   (if where-i-was
;;     (where-i-was)
;;     (let/cc really-return
;;       (define (return the-answer)
;;         (let/cc where-i-am
;;           (set! where-i-was
;;                 where-i-am)
;;           (really-return the-answer)))
;;       ;; where-i-am will go--->
;;       (return "Kirtland")
;;       ;; ---> right here --->
;;       (return "Far West")
;;       (return "Nauvoo")
;;       (return "SLC")
;;       (return "The Celestial Kingdom (aka Provo)")
;;       #f)))

(define (make-generator
         generator-body)
  (define where-i-was #f)
  (define (whered-they-go)
    (if where-i-was
      (where-i-was)
      (let/cc really-return
        (define (return the-answer)
          (let/cc where-i-am
            (set! where-i-was
                  where-i-am)
            (really-return the-answer)))
        (generator-body return)
        #f)))
  whered-they-go)

;; (+ (prompt "First")
;;    (prompt "Second"))

(define whered-they-go
  (make-generator
   (λ (return)
     (return "Kirtland")
     (return "Far West")
     (return "Nauvoo")
     (return "SLC")
     (return "The Celestial Kingdom (aka Provo)"))))

;; Infinite loop:
;; (list (whered-they-go) (whered-they-go))

;; really-return will go --->
(whered-they-go)
;; ---> right here
(whered-they-go)
(whered-they-go)
(whered-they-go)
(whered-they-go)
(whered-they-go)
(whered-they-go)
(whered-they-go)

;; (set! where-i-was #f)
;; (whered-they-go)

(define (list->generator l)
  (make-generator
   (λ (return)
     (for-each (λ (x) (return x)) l))))

(define some-l-g
  (list->generator '(1 2 3)))

(some-l-g)
(some-l-g)
(some-l-g)

(define (tree-walk f l)
  (cond
    [(empty? l)
     (void)]
    [(cons? l)
     (tree-walk f (first l))
     (tree-walk f (rest l))]
    [else
     (f l)]))

(define tree
  (list 1
        (list 2 3)
        (list 5 6 (list 7 8) 9)
        (list 10)))

(tree-walk displayln tree)

(define (tree->generator t)
  (make-generator
   (λ (return)
     (tree-walk return t))))

(define tree-g
  (tree->generator tree))

(tree-g)
(tree-g)
(tree-g)

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
(what-are-the-odds?)

"END"
