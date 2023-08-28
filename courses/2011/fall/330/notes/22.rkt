#lang racket
(require web-server/servlet
         web-server/page
         web-server/dispatch
         web-server/servlet-env)

(define (ask-for-first-number req)
  (send/back
   (response/xexpr
    `(html (head (title "Give me the first number"))
           (body
            (form ([action ,(->url ask-for-second-number)])
                  "First number: " (input ([name "n"]))
                  (input ([type "submit"]))))))))

(define (ask-for-second-number req)
  (send/back
   (response/xexpr
    `(html (head (title "Give me the second number"))
           (body
            (form ([action ,(->url give-the-sum (string->number (get-binding 'n req)))])
                  "Second number: " (input ([name "n"]))
                  (input ([type "submit"]))))))))

(define (give-the-sum req first-n)
  (send/back
   (response/xexpr
    `(html (head (title "Give me the second number"))
           (body
            "The answer is "
            ,(number->string
              (+ first-n
                 (string->number (get-binding 'n req)))))))))

(define-values (dispatch ->url)
  (dispatch-rules
   [("/second") ask-for-second-number]
   [("/sum" (integer-arg)) give-the-sum]
   [else ask-for-first-number]))

(serve/servlet dispatch
               #:servlet-regexp #rx""
               #:port 9000)
