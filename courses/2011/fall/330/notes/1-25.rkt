#lang racket

(call-with-current-continuation
 (lambda (my-kontinuation)
   5))

(call/cc
 (lambda (my-kontinuation)
   5))

(let/cc
 my-kontinuation
 5)

(+ 2
   (let/cc
    my-kontinuation
    5))
"should be"
7

(+ 2
   (let/cc
    my-kontinuation
    (my-kontinuation 5)))
"should be"
7

(+ 2
   (let/cc
    my-kontinuation
    (* 2
       (my-kontinuation 5))))
"should be"
7

(define (f x y)
  (let/cc
   return
   (when (> x 0)
         (return -1))
   (when (> y 0)
         (return -1))
   (+ x y)))

(f -5 -5) "should be" -10
(f 5 -5) "should be" -1
(f -5 5) "should be" -1

(define-syntax-rule
  (define/return (f . x) e ...)
  (define (f . x)
    (let/cc return e ...)))

(define current-raise #f)
(define (catch-exns f handler)
  (let/cc my-raise
          (set! current-raise
                (lambda (x)
                  (my-raise (handler x))))
          (f)))

(define (g x)
  (+ 4 (* 4
          (if (zero? x)
              (current-raise "error")
              (/ 1 x)))))

(catch-exns (lambda () (g 0))
            (lambda (x) (printf "Threw ~a\n" x)))
"should be" (void)

;;;;;;;

(define (make-a-producer the-real-producer)
  (let ()
    (define the-next-stop #f)
    (lambda ()
      (if the-next-stop
          (let/cc next-return
                  (the-next-stop next-return))
          (let/cc first-return
                  (define the-next-return first-return)
                  (define (return x)
                    (set! the-next-return
                          (let/cc the-rest
                                  (set! the-next-stop the-rest)
                                  (the-next-return x)))
                    #f)
                  (the-real-producer return)
                  (the-next-return #f))))))

(define whered-they-go
  (make-a-producer
   (lambda (return)
     (return "Kirtland")
     (return "Zion")
     (return "Far West")
     (return "Nauvoo")
     (return "Council Bluffs")
     (return "SLC"))))

;;(lambda (x) (list x (whered-they-go)))

(list (whered-they-go) (whered-they-go)) 
"should be"
(list "Kirtland" "Zion")

(whered-they-go) "should be" "Far West"
(whered-they-go) "should be" "Nauvoo"
(whered-they-go) "should be" "Council Bluffs"
(whered-they-go) "should be" "SLC"
(whered-they-go) "should be" #f
(whered-they-go) "should be" #f

(define what-are-the-odds
  (make-a-producer
   (lambda (return)
     (define (go-from-n-to-the-next-one n)
       (return n)
       (go-from-n-to-the-next-one (+ 2 n)))
     (go-from-n-to-the-next-one 1))))

(what-are-the-odds) "should be" 1
(what-are-the-odds) "should be" 3
(what-are-the-odds) "should be" 5
(what-are-the-odds) "should be" 7
(what-are-the-odds) "should be" 9
