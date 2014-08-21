#lang plai

;; YESTERDAY
;; - The Web is programming without an automatic stack
;; - A stack is control + data

;; TODAY
;; - Closures are control + data         [0:07]
;; -- convert add-two-numbers.com        [0:14]
;; - add-many-numbers.exe                [0:25]
;; - add-many-numbers.com                [0:35]
;; -- remove closures                    [0:40]
;; - What is Web style?
;; -- order of evalaution & sequential
;; -- global
;; -- no-closures                        [0:50]

;; CONSOLE
(define (prompt text)
  (displayln text)
  (read))
(define (add-two-numbers.exe)
  (displayln
   (+ (prompt "First number:")
      (prompt "Second number:"))))

;; WEB
(define (web-prompt text whose-next more)
  (web-displayln text whose-next more))
(define (web-displayln text whose-next more)
  (printf "<html><input type=\"hidden\" value=\"~a\"/>\n~a</html>\n"
          more text)
  (whose-next (read) more)
  (error 'http "I'm stateless"))
(define (add-two-numbers.com)
  (web-prompt
   "First number:"
   add-two-numbers.com/first.cgi
   #f))

(define (add-two-numbers.com/first.cgi
         first-number junk)
  (web-prompt
   "Second number:"
   add-two-numbers.com/second.cgi
   first-number))

(define (add-two-numbers.com/second.cgi
         second-number first-number)
  (web-displayln
   (+ first-number
      second-number)
   void
   #f))

;;(add-two-numbers.com)

;; WEB w/ closures
(define (web-prompt/clo text next-clo)
  (web-displayln/clo text next-clo))
(define (web-displayln/clo text next-clo)
  (printf "<html><input type=\"hidden\" value=\"~a\"/>\n~a</html>\n"
          next-clo text)
  (next-clo (read))
  (error 'http "I'm stateless"))
(define (add-two-numbers.com/clo)
  (web-prompt/clo
   "First number:"
   (λ (first-number)
     (web-prompt/clo
      "Second number:"
      (λ (second-number)
        (web-displayln/clo
         (+ first-number
            second-number)
         void))))))

;; (add-two-numbers.com/clo)

(define (sum l)
  (foldr + 0 l))
(define (build-list n f)
  (cond
    [(zero? n)
     empty]
    [else
     ;; We are returning here
     (define rest (build-list (sub1 n) f))
     ;; We are returning here
     (cons (f n) rest)]))
(define (add-many-numbers.exe)
  (define how-many
     ;; We are returning here
    (prompt "How many numbers?"))
  (displayln
   (format
    "The sum of your ~a numbers is: ~a"
    how-many
    (sum
     ;; We are returning here
     (build-list
      how-many
      (λ (i)
        ;; We are returning here
        (prompt
         (format "~a# number:" i))))))))

;; (add-many-numbers.exe)

(define (web-build-list/clo n web-f clo)
  (cond
    [(zero? n)
     (clo empty)]
    [else
     (web-build-list/clo
      (sub1 n) web-f
      (λ (rest)
        (web-f
         n
         (λ (first)
           (clo (cons first rest))))))]))
(define (add-many-numbers.com/clo)
  (web-prompt/clo
   "How many numbers?"
   (λ (how-many)
     (web-build-list/clo
      how-many
      (λ (i clo)
        (web-prompt/clo
         (format "~a# number:" i)
         clo))
      (λ (the-numbers)
        (web-displayln/clo
         (format
          "The sum of your ~a numbers is: ~a"
          how-many
          (sum
           the-numbers))
         void))))))

;; (add-many-numbers.com/clo)

(define (web-build-list n web-f next next-data)
  (cond
    [(zero? n)
     (next empty next-data)]
    [else
     (web-build-list
      (sub1 n) web-f
      web-build-list/got-rest.cgi
      (list web-f n next next-data))]))
(define (web-build-list/got-rest.cgi
         rest data)
  (match-define (list web-f n next next-data) data)
  (web-f
   n
   web-build-list/got-one.cgi
   (list rest next next-data)))
(define (web-build-list/got-one.cgi
         first data)
  (match-define (list rest next next-data) data)
  (next (cons first rest) next-data))

(define (add-many-numbers.com)
  (web-prompt
   "How many numbers?"
   add-many-numbers.com/how-many.cgi
   #f))
(define (add-many-numbers.com/how-many.cgi
         how-many junk)
  (web-build-list
   how-many
   add-many-numbers.com/get-one.cgi
   add-many-numbers.com/got-numbers.cgi
   how-many))
(define (add-many-numbers.com/get-one.cgi
         i next next-data)
  (web-prompt
   (format "~a# number:" i)
   next next-data))
(define (add-many-numbers.com/got-numbers.cgi
         the-numbers how-many)
  (web-displayln
   (format
    "The sum of your ~a numbers is: ~a"
    how-many
    (sum
     the-numbers))
   void
   #f))

(add-many-numbers.com)

;; (... waiting for ans ... (f arg ...))
;; ->
;; (f/clo arg .... (λ (ans) (... waiting for ans ... )))
