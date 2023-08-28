#lang racket/base
(require racket/match)

(define (ask-for-number label)
  (printf "Give me the ~a number\n" label)
  (read))
(define (add-two-numbers.exe)
  (define f (ask-for-number "first"))
  (define s (ask-for-number "second"))
  (displayln (+ f s)))

#;(add-two-numbers.exe)


;; HTTP
;; Client > Server : GET add-two-numbers.com/
;; Server < Client : <html>..first?..</html>
;; Connection closes

;; Client > Server : GET add-two-numbers.com/heres-the-first-number.html?n=20
;; Server < Client : <html>...second?...</html>


(define display! (box #f))
(define (web-display msg)
  ((unbox display!) msg))
(define (web-ask q where-to-next)
  (web-display (cons where-to-next q)))
(define (web-read) (read))

(define (browse url ans)
  (match-define (cons where-to-next msg)
    (let/cc k
      (set-box! display! k)
      (url ans)))
  (displayln msg)
  where-to-next)

;; Node.JS is like this:
(define (<ask-for-number> label what-next)
  (web-ask (format "Give me the ~a number" label) what-next))
(define (add-two-numbers.com ignored)
  (<ask-for-number>
   "first"
   (λ (first-num)
     (<ask-for-number>
      "second"
      (λ (second-num)
        (web-ask (+ first-num second-num) void))))))

(define get-first-number.html (browse add-two-numbers.com #f))
(define get-snd-number.html (browse get-first-number.html 20))
(browse get-snd-number.html 24)

(define (ask-for-number/awesome label)
  (let/cc what-next
    (web-ask (format "Give me the ~a number" label)
             what-next)))
(define (add-two-numbers.com/awesome ignored)
  (define f (ask-for-number/awesome "first"))
  (define s (ask-for-number/awesome "second"))
  (web-ask (+ f s) void))

(define get-first-number.html/awesome (browse add-two-numbers.com/awesome #f))
(define get-snd-number.html/awesome (browse get-first-number.html/awesome 20))
(browse get-snd-number.html/awesome 24)
