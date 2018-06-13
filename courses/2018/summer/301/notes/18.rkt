#lang web-server/insta

(define (ask-for-num which)
  (define ans-req
    (send/suspend
     (λ (k-url)
       (response/xexpr
        `(html
          (body
           (p "Give me the " ,which " number")
           (form ([action ,k-url])
                 (input ([type "text"] [name "number"]))
                 (input ([type "submit"])))))))))
  (string->number
   (extract-binding/single
    'number
    (request-bindings ans-req))))
(define (start req)
  (send/back
   (response/xexpr
    `(html
      (body
       (p "The answer is "
          ,(number->string
            (for/sum ([i (in-range (ask-for-num "How many"))])
              (ask-for-num (number->string i))))))))))
