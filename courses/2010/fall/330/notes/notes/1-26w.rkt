#lang web-server/insta

(define (prompt question)
  (string->number
   (extract-binding/single
    'num
    (request-bindings
     (send/suspend
      (λ (kont-as-url)
        `(html (body (form ([action ,kont-as-url])
                           ,question (input ([name "num"])))))))))))

(define (show fmt a)
  (format fmt a))

(define (start req)
  (define how-many (prompt "How many numbers?"))
  (show "The sum of your numbers is ~a."
        (foldr + 0
               (build-list how-many
                           (λ (i) (prompt "Number:"))))))