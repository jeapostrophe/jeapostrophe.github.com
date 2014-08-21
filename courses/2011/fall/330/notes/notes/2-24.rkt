#lang racket

(define (read-from-the-user prompt v)
    (printf "~a\n" prompt)
    ;; (read)
    v)
(define (read-from-the-user/k prompt the-guy)
  (printf "You are really calling the continuation one\n")
  (printf "~a\n" prompt)
  (the-guy (read))
  (error 'read-from-the-user/k "The program has died"))

;; Add Two Numbers dot com (C code)
(let ()
  (define num1 (read-from-the-user "Gimme the first number:"
                                   10))
  (define num2 (read-from-the-user "Gimme the second number:"
                                   14))
  (printf "The answer is ~a\n"
          (+ num1 num2)))

;; Add Two Numbers dot com (simulated Web code)
(when #f
      (let ()
        (read-from-the-user/k
         "Gimme the first number:"
         (lambda (num1)
           (read-from-the-user/k
            "Gimme the second number:"
            (lambda (num2)
              (printf "The answer is ~a\n"
                      (+ num1 num2))
              (error 'really-over)))))))

;; Continuation Passing Style (or CPS)

(define FINAL-COUNTDOWN
  (lambda (final-ans)
    (error 'dun-da-dun-dun-dun-da-da-dun-dun "The answer was ~a" final-ans)))

;; CPS : program -> program[CPS]
(require (for-syntax syntax/parse))
(define-syntax (CPS p)
  (syntax-parse
   p
   #:literals (printf + read-from-the-user lambda)

   [(CPS n:nat)
    (syntax
     (lambda (guy-who-wants-a-number-k)
       (guy-who-wants-a-number-k n)))]
   [(CPS some-string:str)
    (syntax
     (lambda (guy-who-wants-a-string-k)
       (guy-who-wants-a-string-k some-string)))]
   [(CPS read-from-the-user)
    (syntax
     (lambda (guy-who-wants-read-k)
       (guy-who-wants-read-k
        (lambda (prompt fake-value guy-who-called-read-k)
          (read-from-the-user/k prompt guy-who-called-read-k)))))]
   [(CPS printf)
    (syntax
     (lambda (guy-who-wants-printf-k)
       (guy-who-wants-printf-k
        (lambda (fmt arg guy-who-called-printf-k)
          (guy-who-called-printf-k (printf fmt arg))))))]
   ;; Homework: Change names from printf to +
   [(CPS +)
    (syntax
     (lambda (guy-who-wants-printf-k)
       (guy-who-wants-printf-k
        (lambda (fmt arg guy-who-called-printf-k)
          (guy-who-called-printf-k (+ fmt arg))))))]

   [(CPS (lambda (x ...) b))
    (syntax
     (lambda (guy-who-wants-a-lambda-k)
       (guy-who-wants-a-lambda-k
        (lambda (x ... guy-who-called-lambda-k)
          ((CPS b) guy-who-called-lambda-k)))))]

   [(CPS (f arg0 arg1))
    (syntax
     (lambda (funcalls-k)
       ((CPS f)
        (lambda (f/k)
          ((CPS arg0)
           (lambda (arg0v)
             ((CPS arg1)
              (lambda (arg1v)
                (f/k arg0v arg1v funcalls-k)))))))))]
   [(CPS (f arg0))
    (syntax
     (lambda (funcalls-k)
       ((CPS f)
        (lambda (f/k)
          ((CPS arg0)
           (lambda (arg0v)
             (f/k arg0v funcalls-k)))))))]
   [(CPS n:id)
    (syntax
     (lambda (guy-who-wants-an-id-k)
       (guy-who-wants-an-id-k n)))]))

(when #f
((CPS
  (printf "The answer is ~a\n"
          (+ (read-from-the-user "Gimme the first number:"
                                 10)
             (read-from-the-user "Gimme the second number:"
                                 15))))
 FINAL-COUNTDOWN))

(when #f
((CPS
  (printf "The answer is ~a\n"
          (+ (+ (read-from-the-user "Gimme the first number:"
                                    10)
                (+ (read-from-the-user "Gimme the first number:"
                                       10)
                   (read-from-the-user "Gimme the second number:"
                                       15)))
             (+ (read-from-the-user "Gimme the first number:"
                                    10)
                (+ (+ (read-from-the-user "Gimme the first number:"
                                          10)
                      (read-from-the-user "Gimme the second number:"
                                          15))
                   (read-from-the-user "Gimme the second number:"
                                       15))))))
 FINAL-COUNTDOWN))

((CPS
  (printf "The answer is ~a\n"
          ;; (with (x xe) e) => ((lambda (x) e) xe)
          ((lambda (num1)
             (+ num1
                (read-from-the-user "Gimme the second number:"
                                    15)))
           (read-from-the-user "Gimme the first number:"
                               10))))
 FINAL-COUNTDOWN)

;; XXX CPS any program
;; XXX CPS transform
;; XXX Fischer transform
