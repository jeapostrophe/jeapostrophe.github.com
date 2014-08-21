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
  (printf "<html><input action=\"~a\" type=\"hidden\" value=\"~a\"/>\n~a</html>\n" whose-next more text)
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
(define (web-prompt/clo text clo)
  (web-displayln/clo text clo))
(define (web-displayln/clo text clo)
  (printf "<html><input type=\"hidden\" value=\"~a\"/>\n~a</html>\n" clo text)
  (clo (read))
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

;; (... wants ans ... (f arg ...))
;; ->
;; (f/clo arg ... (λ (ans) ... wants ans ...))

;; -> cgi ->
;; "lambda lifting"
;; take a lambda and add arguments as you go out of the program

(define (sum l)
  (foldr + 0 l))
(define (build-list n f)
  (cond
    [(zero? n)
     empty]
    [else
     (define the-rest
       (build-list (sub1 n) f))
     (cons (f n)
           the-rest)]))
(define (add-many-numbers.exe)
  (define how-many
    (prompt "How many numbers?"))
  (displayln
   (format
    "Your ~a numbers sum to ~a"
    how-many
    (sum
     (build-list
      how-many
      (λ (i)
        (prompt (format "~ath number?" i))))))))

;; (add-many-numbers.exe)

(define (web-build-list/clo n web-f/clo clo)
  (cond
    [(zero? n)
     (clo empty)]
    [else
     (web-build-list/clo
      (sub1 n) web-f/clo
      (λ (the-rest)
        ;; (f arg ...) -> (f/clo arg ... (λ (ans) ...))
        (web-f/clo
         n
         (λ (the-first)
           (clo
            (cons the-first
                  the-rest))))))]))
(define (add-many-numbers.com/clo)
  (web-prompt/clo
   "How many numbers?"
   (λ (how-many)
     (web-build-list/clo
      how-many
      (λ (i clo)
        (web-prompt/clo
         (format "~ath number?" i)
         clo))
      (λ (the-numbers)
        (web-displayln/clo
         (format
          "Your ~a numbers sum to ~a"
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
      web-build-list.com/got-rest.cgi
      (list web-f n next next-data))]))
(define (web-build-list.com/got-rest.cgi
         the-rest data)
  (match-define (list web-f n next next-data)
                data)
  (web-f
   n
   web-build-list.com/got-one.cgi
   (list next next-data the-rest)))
(define (web-build-list.com/got-one.cgi
         the-first
         data)
  (match-define (list next next-data the-rest)
                data)
  (next
   (cons the-first
         the-rest)
   next-data))
(define (add-many-numbers.com)
  (web-prompt
   "How many numbers?"
   add-many-numbers.com/got-how-many.cgi
   #f))

(define (add-many-numbers.com/got-how-many.cgi
         how-many junk)
  (web-build-list
   how-many
   add-many-numbers.com/get-one.cgi
   add-many-numbers.com/caught-em-all.cgi
   how-many))
(define (add-many-numbers.com/caught-em-all.cgi
         the-numbers how-many)
  (web-displayln
   (format
    "Your ~a numbers sum to ~a"
    how-many
    (sum
     the-numbers))
   void
   #f))
(define (add-many-numbers.com/get-one.cgi
         i next next-data)
  (web-prompt
   (format "~ath number?" i)
   next next-data))

(add-many-numbers.com)
