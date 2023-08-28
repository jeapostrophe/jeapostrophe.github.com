#lang racket

;; (define (loop)
;;   (when (f)
;;         (g)
;;         (loop)))

;; (define (loop/k k)
;;   (f/k (lambda (fv)
;;          (when fv
;;                (g/k
;;                 (lambda (gv)
;;                   (loop/k k)))))))

(call-with-current-continuation
 (lambda (my-continuation)
   5))
"should be"
5

(call/cc
 (lambda (my-continuation)
   5))
"should be"
5

(let/cc
 my-continuation
 5)
"should be"
5


(+ 2
   (let/cc
    my-continuation ;; = (lambda (x) (exit (+ 2 x)))
    5))
"should be"
7

(+ 2
   (let/cc
    my-continuation
    (my-continuation 5)))
"should be"
7

(+ 2
   (let/cc
    my-continuation
    (+ 2 (* 5 (my-continuation 5)))))
"should be"
7

(define (f x y)
  (let/cc
   return
   (when (> x 0)
         (return "x" -1))
   (when (> y 0)
         (return "y" -1))
   (+ x y)))

(f -5 -5) "should be" -10
(f 5 -5) "should be" -1
(f -5 5) "should be" -1

(lambda (x) x)
(values 4 5)
(lambda (x y) #f)

(define throw #f)
(define (catch body handler)
  (let/cc
   outside
   (set! throw
         (lambda (x)
           (outside (handler x))))
   (body)))

(define (g x)
  (+ 2
     (* 3
        (if (zero? x)
            (throw "div by zero")
            (/ 1 x)))))

(catch (lambda () (g 0))
       (lambda (x) "exn thrown"))
"should be"
"exn thrown"

;;;;;;;;;;;;;;;

(define (make-a-producer real-producer)
  (let ()
    (define next-stop #f)
    (lambda ()
      (if next-stop
          (let/cc 
           next-return
           (next-stop next-return))
          (let/cc
           first-return
           (define real-return first-return)
           (define (return x)
             (set! real-return
                   (let/cc 
                    the-next-stop
                    (set! next-stop the-next-stop)
                    (real-return x))))
           (real-producer return)
           (real-return #f))))))

(define whered-they-go
  (make-a-producer
   (lambda (return)     
     (begin
       (return "Kirtland")
       (return "Zion")
       (return "Nauvoo")
       (return "SLC")))))

;; wrong-return = (lambda (x) (list x (whered-they-go)))
(list (whered-they-go) (whered-they-go))
"should be"
(list "Kirtland" "Zion")

(whered-they-go) "should be" "Nauvoo"
(whered-they-go) "should be" "SLC"
(whered-they-go) "should be" #f
(whered-they-go) "should be" #f

(define what-are-the-odds
  (make-a-producer
   (lambda (return)
     (define (start-at-this-and-go-to-infinity-and-beyond n)
       (return n)
       (start-at-this-and-go-to-infinity-and-beyond (+ 2 n)))
     (start-at-this-and-go-to-infinity-and-beyond 1))))

(what-are-the-odds) "should be" 1
(what-are-the-odds) "should be" 3
(what-are-the-odds) "should be" 5
(what-are-the-odds) "should be" 7
(what-are-the-odds) "should be" 9

(define (list->producer l)
  (make-a-producer
   (lambda (return)
     (for-each return l))))

(define p (list->producer '(1 2 3 4)))

(p) (p) (p) (p) (p)
