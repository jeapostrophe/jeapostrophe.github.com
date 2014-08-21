#lang racket

"BOO" "LEAN"

"BOO ALIEN"

"BOOLIEN"

;; Add Two Numbers dot com (C code)
(let ()
  (define (read-from-the-user prompt v)
    (printf "~a\n" prompt)
    ;; (read)
    v)

  (define num1 (read-from-the-user "Gimme the first number:"
                                   10))
  (define num2 (read-from-the-user "Gimme the second number:"
                                   14))
  (printf "The answer is ~a\n"
          (+ num1 num2)))

;; Add Two Numbers dot com (simulated Web code)
(let ()
  (define (tell-the-user-to-tell-this-guy
           prompt v
           this-guy
           more-stuff)
    "<form action=this-guy> <input type=hidden value=more-stuff> prompt <input name=this-thing /> <input type=submit /> ..."
    (printf "~a\n" prompt)
    (this-guy more-stuff v))

  (define (ron.pl num1 num2)
      (printf "The answer is ~a\n"
              (+ num1 num2)))
  
  (define (joe.php dont-care num1)    
    (tell-the-user-to-tell-this-guy
     "Gimme the second number:"
     14
     ron.pl num1))

  (define (me.rkt)
    (tell-the-user-to-tell-this-guy
     "Gimme the first number:"
     10
     joe.php #f))

  (me.rkt))

;; Add Two Numbers dot com (real Web code)
(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         web-server/page)

(let ()
  (define (ron.pl req num1)
    (define num2 (string->number (get-binding "num2" req)))
    (send/back
     (response/xexpr
      `(html (head (title "Here's the answer"))
             (body
              (p
               "The answer is "
               ;;"<+>num1 num2</+>"
               ;;(+ num1 num2)
               ;; "Your answer is $THEANSWERERRR!!!
               ;; "Your answer is $a + b
               ;; "Your answer is ${a + b}
               ,(number->string
                 (+ num1 num2))))))))

  (define num1 #f)
  
  (define (joe.php req)
    (define num1 (string->number (get-binding "num1" req)))
    (send/back
     (response/xexpr
      `(html (head (title "Gimme the second number"))
             (form ([action ,(fun-call->url ron.pl num1)])
                   (input ([name "num2"] [type "text"]))
                   (input ([type "submit"])))))))

  (define (me.rkt req)
    (send/back
     (response/xexpr
      `(html (head (title "Gimme the first number"))
             (form ([action ,(fun-call->url joe.php)])
                   (input ([name "num1"] [type "text"]))
                   (input ([type "submit"])))))))

  (define-values
    (dispatch fun-call->url)
    (dispatch-rules
     [("my-brother-joe") joe.php]
     [("my-uncle-ron" "where the first number iiiiss" (integer-arg)) ron.pl]
     [else me.rkt]))

  (serve/servlet dispatch
                 #:servlet-regexp #rx""
                 #:port 9000))
