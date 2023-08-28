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
(define (show fmt a)
  (send/back `(html (body ,(format fmt a)))))

; This is stateless
#;(define (start req)
  (show "The sum of your numbers is ~a."
        (+ (prompt "First number:")
           (prompt "Second number:"))))

(define first-n #f)
(define second-n #f)
(define (start req)
  (define maybe-num 
    (with-handlers ([exn? (λ (x) #f)])
      (extract-binding/single 'num (request-bindings req))))
  (when maybe-num
    (define num (string->number maybe-num))
    (cond [(not first-n) (set! first-n num)]
          [(not second-n) (set! second-n num)]))
  (cond
    [(not first-n)
     (prompt "First number:")]
    [(not second-n)
     (prompt "Second number:")]
    [else
     (show "The sum of your numbers is ~a."
           (+ first-n second-n))]))

|#

(define current-browser-tab (box "google.com"))
(define (prompt-with-memory p after-fun)
  (display p)
  (set-box! current-browser-tab after-fun)
  (error 'http "Connection closed!"))
(define (submit n)
  ((unbox current-browser-tab) n))

(define (show fmt a)
  (printf fmt a)
  (error 'http "Connection closed!"))

(define (start)
  (define (what-happens-after-we-get-the-first-number first-n)
    (define (and-then... second-n)
      (show "The sum of your numbers is ~a."
            (+ first-n
               second-n)))
    (prompt-with-memory
     "Second number:"
     and-then...))
  (prompt-with-memory 
   "First number:"
   what-happens-after-we-get-the-first-number))

(start)