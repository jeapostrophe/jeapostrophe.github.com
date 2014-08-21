#lang racket

;; Add Two Numbers . com (wish)
(local ()
       (define (prompt&read p v)
         (printf "~a\n" p)
         v)

       (define num1
         (prompt&read "Gimme the first number"
                      10))
       (define num2
         (prompt&read "Gimme the second number"
                      14))
       (printf "Hey, the sum is ~a\n"
               (+ num1 num2)))

;; Add Two Numbers . com (demo)
(local ()
       (define (prompt&read&tell-my-brother-your-answer+this
                p v brother this)
         (printf "~a\n" p)
         (brother this v))

       (define (then-ask-for-second-number dont-care num1)
         (prompt&read&tell-my-brother-your-answer+this
          "Gimme the second number"
          14
          then-return-the-answer
          num1))

       (define (then-return-the-answer num1 num2)
         (printf "Hey, the sum is ~a\n"
                 (+ num1 num2)))

       (prompt&read&tell-my-brother-your-answer+this
        "Gimme the first number"
        10
        then-ask-for-second-number
        #f))


;; Add Two Numbers . com
(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         web-server/page)

(let ()
  "My name is $YOURNAMEHERE"

  (define (first-ask-for-the-first-number req)
    (send/back
     (response/xexpr
      `(html (head (title "Gimme the first number"))
             (body
              (form ([action ,(fun-call->url then-ask-for-second-number)])
                    "Put it here: " (input ([type "text"] [name "num1"]))
                    (input ([type "submit"]))))))))

  (define num1 #f)

  (define (then-ask-for-second-number req)
    (define num1 (string->number (get-binding "num1" req)))
    (send/back
     (response/xexpr
      `(html (head (title "Gimme the second number"))
             (body
              (form ([action ,(fun-call->url then-return-the-answer num1)])
                    "Put it here: " (input ([type "text"] [name "num2"]))
                    (input ([type "submit"]))))))))

  (define num2 #f)

  (define (then-return-the-answer req num1)
    (define num2 (string->number (get-binding "num2" req)))
    (send/back
     (response/xexpr
      `(html (head (title "The final answer"))
             (body
              ,(number->string (+ num1 num2)))))))

  ;; url -> calls the appropriate function
  (define-values
    (dispatch fun-call->url)
    (dispatch-rules
     [("my-uncle" (integer-arg)) then-return-the-answer]
     [("my-brother") then-ask-for-second-number]
     [else first-ask-for-the-first-number]))

  (fun-call->url then-return-the-answer 10)

  (when #f
    (serve/servlet dispatch
                   #:port 9000
                   #:servlet-regexp #rx"")))


;; Add Many Numbers dot com

(let ()
  (define (prompt&read p)
    (printf "~a\n" p)
    (read))
  (define (prompt&read&tell-this-guy p this-guy)
    (printf "~a\n" p)
    (this-guy (read)))

  ;; build-list&tell-this-guy : nat (nat answer-receiver -> nothing) answer-receiver -> nothing
  (define (build-list&tell-this-guy n f this-guy)
    (if (zero? n)
        (this-guy empty)
        (f n
           (lambda (users-answer)
             (build-list&tell-this-guy
              (sub1 n) f
              (lambda (rest-of-the-numbers)
                (this-guy
                 (cons users-answer
                       rest-of-the-numbers))))))))
  (prompt&read&tell-this-guy
   "How many numbers do you want to add?"
   (lambda (how-many-numbers)
     (build-list&tell-this-guy
      how-many-numbers
      (lambda (i this-guy)
        (prompt&read&tell-this-guy
         (format "Give me the ~ath number"
                 i)                      
         this-guy))
      (lambda (all-of-the-numbers)
       (define (sum l)
         (cond [(empty? l) 0]
               [else (+ (first l) (sum (rest l)))]))
       (printf "Your sum is ~a\n"
               (sum all-of-the-numbers)))))))

;; Rule
#|
 when you see a function call

 (g (h (x ... (f a ...) ...)))
 =>
 (f a ...
    (lambda (ans) (g (h (x ... ans ...)))))

 The point is, functions do not return, like users do not directly
 give back answers. Instead, they send the answer to the guy you tell them to

 e = (e e ...)
   | (lambda (x ...) e)
   | x

=>

 s.e. = (lambda (x ...) e)
      | x
 e    = (se se ...)
|#

#|
;; C code
(f (g x) (h x) (y z))
;; Web code
(g&tell-this-guy
 x
 (lambda (gs-ans)
   (h&tell-this-guy
    x
    (lambda (hs-ans)
      (y&tell-this-guy
       z
       (lambda (ys-ans)
         (f gs-ans hs-ans ys-ans)))))))
|#

;; XXX Average Many Temperatures

;; XXX selects order of operations, global, and sequences


;; Explicit Stacks are called CONTINUATIONS
;; Continuation Passing Style
