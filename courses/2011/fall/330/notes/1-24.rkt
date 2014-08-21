#lang racket

(define (read-from-the-user prompt v)
    (printf "~a\n" prompt)
    ;; (read)
    v)
(define (read-from-the-user/k prompt v the-guy)
    (printf "~a\n" prompt)
    (the-guy (read))
    ;;(the-guy v)
    (error 'read-from-the-user "The program has died"))

;; Add Two Numbers dot com (C code)
(let ()
  ;;  (with (x xe) e) => ((lambda (x) e) xe)
  ((lambda (num1) 
     ((lambda (num2) 
        (printf "The answer is ~a\n"
                (+ num1 num2)))
      (read-from-the-user "Gimme the second number:"
                          14)))
   (read-from-the-user "Gimme the first number:"
                       10)))

;; Add Two Numbers dot com (simulated Web code)
(when #f
      (let ()
        (read-from-the-user/k
         "Gimme the first number:"
         5
         (lambda (num1)
           (read-from-the-user/k
            "Gimme the second number:"
            4
            (lambda (num2)
              (printf "The answer is ~a\n"
                      (+ num1 num2))
              (error 'really-over)))))))

;; Continuation Passing Style

;; CPS : program (not in CPS) -> program (is in CPS)
(require (for-syntax syntax/parse))
(define-syntax (CPS p-stx)
  (syntax-parse
   p-stx
   #:literals (lambda read-from-the-user printf +)
   ;; e -> (lambda (k) e giving the answer to k)
   [(CPS (read-from-the-user prompt fake-val))
    (syntax
     (lambda (k)       
       (read-from-the-user/k prompt fake-val k)))]
   [(CPS (printf fmt arg))
    (syntax
     (lambda (k)
       ((CPS fmt)
        (lambda (fmtv)
          ((CPS arg)
           (lambda (argv)
             (k (printf fmtv argv))))))))]
   [(CPS (+ fmt arg))
    (syntax
     (lambda (k)
       ((CPS fmt)
        (lambda (fmtv)
          ((CPS arg)
           (lambda (argv)
             (k (+ fmtv argv))))))))]
   [(CPS s:str)
    (syntax
     (lambda (k)
       (k s)))]
   [(CPS x:id)
    (syntax
     (lambda (k)
       (k x)))]
   [(CPS n:nat)
    (syntax
     (lambda (k)
       (k n)))]
   [(CPS (lambda (x ...) e))
    (syntax
     (lambda (lambdas-k)
       (lambdas-k 
        (lambda (x ... funcalls-k)
          ((CPS e) funcalls-k)))))]
   [(CPS (f a))
    (syntax    
     (lambda (k)
       ((CPS f)
        (lambda (fv)
          ((CPS a)
           (lambda (av)
             (fv av k)))))))]))

(define OS-K
  (lambda (x) 
    (printf "Exit called with ~v" x)))

"Here's our example:"

(when #f
      (let ()
        ((CPS
          ((lambda (num1) 
             ((lambda (num2) 
                (printf "The answer is ~a\n"
                        (+ num1 num2)))
              (read-from-the-user "Gimme the second number:"
                                  14)))
           (read-from-the-user "Gimme the first number:"
                               10)))
         OS-K)))

(when #f
      (let ()
        ((CPS
          (printf "The answer is ~a\n"
                  (+ (read-from-the-user "Gimme the first number:"
                                         10)
                     (read-from-the-user "Gimme the second number:"
                                         14))))
         OS-K)))

(let ()
  ((CPS
    (printf "The answer is ~a\n"
            (+ (read-from-the-user "Gimme a number:"
                                   10)
               (+ (+ (read-from-the-user "Gimme a number:"
                                         10)
                     (read-from-the-user "Gimme a number:"
                                         14))
                  (+ (read-from-the-user "Gimme a number:"
                                         10)
                     (read-from-the-user "Gimme a number:"
                                         14))))))
   OS-K))

;; XXX CPS any program
;; XXX CPS transform
;; XXX Fischer transform

