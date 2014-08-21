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

(serve/servlet dispatch
               #:port 9000
               #:servlet-regexp #rx"")
