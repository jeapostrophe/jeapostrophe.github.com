;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
#|
(call/cc
 (lambda (k)
   ...))

(let/cc k
  ...)

|#

; Basic
(let/cc k
  (k 3))

; Involved
(+ 1 (let/cc k
       (k 3)))

; Exceptions
(define (f n)
  (+ 10
     (* 5
        (let/cc k
          (/ 1 n)))))
(+3 (f 0))

; Pseudo web-read
#;(define (web-read prompt)
  (let/cc k
    (let ([k-id (next-kont-id)])
      (hash-table-put! web-ht k-id k)
      (send-to-browser
       (generate-prompt-with-url-to-kont prompt k-id))
      (halt))))
      

; Producers & Consumers
(define (route-producer send)
  (begin (send 'palmyra)
         (send 'kirtland)
         (send 'far-west)
         (send 'nauvoo)))

