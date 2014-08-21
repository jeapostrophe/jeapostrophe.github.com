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

(define (map/k f l k)
  (if (empty? l)
      (k empty)
      (f (first l)
         (lambda (new-val)
           (map/k f (rest l)
                  (lambda (new-rest)
                    (k (cons new-val
                             new-rest))))))))

(map/k get-one-temp/k
       (list "Palmyra" "Nauvoo")
       (lambda (the-temps)
         (web-display
          (average the-temps))))
