;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
; Testing Web Transformations
(define the-receiver (box 'dummy))
(define receiver-prompt (box 'dummy))

(define (web-display n)
  (printf "Web output: ~a~n" n))

(define (web-read p) (error 'web-read "Can't web read!"))
(define (web-read/k p k)
  (begin (set-box! receiver-prompt p)
         (set-box! the-receiver k)
         (error 'web-read/k "Run (resume) to enter number and simulate clicking Submit")))

(define (resume)
  (begin (display (unbox receiver-prompt))
         ((unbox the-receiver) (read))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average l) (/ (apply + l) (length l)))

(define (get-one-temp c)
  (web-read (format "Temperature in city ~a" c)))

(define (get-one-temp/k c k)
  (web-read/k (format "Temperature in city ~a" c)
              k))

(define (map f l)
  (if (empty? l)
      empty
      (cons (f (first l))
            (map f (rest l)))))

(web-display
 (average
  (map get-one-temp/k
       (list "Palmyra" "Nauvoo"))))
