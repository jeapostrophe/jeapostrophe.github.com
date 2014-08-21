#lang racket

#|
#lang racket/gui

(define (prompt p)
  (read (open-input-string (get-text-from-user "Add Two Numbers.exe" p))))
(define (show fmt . as)
  (message-box "Add Two Numbers.exe" (apply format fmt as)))

(define (start)
  (show "The sum of your numbers is ~a."
        (+ (prompt "First number:")
           (prompt "Second number:"))))

(start)
|#

#|
#lang web-server/insta
(define (prompt p)
  (send/back `(html (body (form ,p (input ([name "num"])))))))
(define (show fmt . as)
  (send/back `(html (body ,(apply format fmt as)))))

#;(define (start req)
  (show "The sum of your numbers is ~a."
        (+ (prompt "First number:") 
           (prompt "Second number:"))))

(define what-stage-am-i-at? 'first)
(define first-n #f)
(define second-n #f)
(define (start req)
  (case what-stage-am-i-at?
    [(second) (set! what-stage-am-i-at? 'add-them)
              (set! first-n (string->number (extract-binding/single 'num (request-bindings req))))
              (prompt "Second number:")]
    [(first) (set! what-stage-am-i-at? 'second)
             (prompt "First number:")]
    [(add-them)
     (set! second-n (string->number (extract-binding/single 'num (request-bindings req))))
     (show "The sum of your numbers is ~a."
           (+ first-n second-n))]))

|#

(define show printf)
(define (prompt p) (display p) (read))

(define resumer (box #f))
(define (prompt-with-stuff-after p after-fun)
  (display p)
  (set-box! resumer after-fun)
  (error 'http "Connection closed!"))
(define (resume ans)
  ((unbox resumer) ans))

(define (where-we-start)
  (prompt-with-stuff-after 
   "First number:"
   what-happens-after-we-get-the-first-number))
(define (what-happens-after-we-get-the-first-number first-n)
  (define (and-after-that... second-n)
    (show "The sum of your numbers is ~a."
          (+ first-n
             second-n)))
  (prompt-with-stuff-after
   "Second number:"
   and-after-that...))

(where-we-start)