#lang web-server/insta

(define (show fmt a)
  (format fmt a))

(define (prompt question)
  (string->number
   (extract-binding/single
    'num
    (request-bindings
     (send/suspend
      (λ (url-that-represents-the-continuation)
        `(html (body (form ([action ,url-that-represents-the-continuation])
                           ,question (input ([name "num"])))))))))))

(define (get-a-number i)
  (prompt "Number:"))
(define (get-n-numbers-and-add-them n)
  (foldr (λ (a b) (+ a b)) 0 (build-list n get-a-number)))

(define (start req)
  (let ([how-many (prompt "How may numbers?")])
    (show "The sum of your numbers is ~a."
          (get-n-numbers-and-add-them how-many))))