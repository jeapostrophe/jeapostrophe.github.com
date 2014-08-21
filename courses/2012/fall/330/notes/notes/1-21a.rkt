#lang plai

;; TODAY
;; - add-many-numbers.exe
;; - add-many-numbers.com

;; EXE
(define (prompt text)
  (displayln text)
  (read))
(define (sum l)
  (foldr + 0 l))
(define (build-list n f)
  (cond
    [(zero? n)
     empty]
    [else
     (define rest (build-list (sub1 n) f))
     (cons (f n) rest)]))
(define (add-many-numbers.exe)
  (define how-many
    (prompt "How many numbers?"))
  (displayln
   (format
    "Your ~a numbers sum to ~a"
    how-many
    (sum
     (build-list how-many
                 (λ (i)
                   (prompt (format "What is number #~a?" i))))))))

(add-many-numbers.exe)

;; WEB
(define (web-prompt text whose-next more)
  (web-displayln text whose-next more))
(define (web-displayln text whose-next more)
  (printf "<html><input type=\"hidden\" value=\"~a\"/>\n~a</html>\n" more text)
  (whose-next (read) more)
  (error 'http "I'm stateless"))
(define (add-many-numbers.com)
  (web-prompt
   "How many numbers?"
   add-many-numbers.com/how-many.cgi
   #f))

(define (web-build-list n f after after-data)
  (cond
    [(zero? n)
     (after empty after-data)]
    [else
     (web-build-list (sub1 n) f
                     web-build-list/got-rest.cgi
                     (list f n after after-data))]))
(define (web-build-list/got-rest.cgi rest f*n*after*after-data)
  (match-define (list f n after after-data) f*n*after*after-data)
  (f n
     web-build-list/got-first.cgi
     (list rest after after-data)))
(define (web-build-list/got-first.cgi first rest*after*after-data)
  (match-define (list rest after after-data) rest*after*after-data)
  (after (cons first rest)
         after-data))

(define (add-many-numbers.com/ask.cgi
         i next data)
  (web-prompt (format "What is number #~a?" i)
              next data))
(define (add-many-numbers.com/how-many.cgi
         how-many junk)
  (web-build-list
   how-many
   add-many-numbers.com/ask.cgi
   add-many-numbers.com/sum.cgi
   how-many))

(define (add-many-numbers.com/sum.cgi
         numbers how-many)
  (web-displayln
   (format
    "Your ~a numbers sum to ~a"
    how-many
    (sum
     numbers))
   void
   #f))

(add-many-numbers.com)
