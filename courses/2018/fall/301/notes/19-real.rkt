#lang web-server/insta

(define (get label)
  (define req
    (send/suspend
     (λ (what-next-url)
       (response/xexpr
        `(html
          (body
           (form
            ([action ,what-next-url])
            "Give me the " ,label " number"
            (input ([name "num"]
                    [type "text"])))))))))
  (string->number
   (extract-binding/single
    'num
    (request-bindings req))))

(define (start req)
  (response/xexpr
   `(html
     (body
      (h1 "The answer is "
          ,(number->string
            (for/sum ([i (in-range (get "how many"))])
              (get (format "number #~a" i)))
            #;(+ (get "first")
               (get "second"))))))))
