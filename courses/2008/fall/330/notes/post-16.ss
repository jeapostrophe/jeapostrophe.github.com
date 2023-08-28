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

; add-two-numbers.com
(define (add-two-numbers)
  (web-display
   (+ (web-read "First number")
      (web-read "Second number"))))

; web-read/k: string (number -> html) -> html

(define (add-two-numbers/k)
  (web-read/k
   "First number"
   (lambda (first-number)
     (web-read/k
      "Second number"
      (lambda (second-number)
        (web-display (+ first-number second-number)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-item-cost-prompt i) (format "Price for: ~a" i))

(define (tally item-list)
  (if (empty? item-list)
      0
      (+ (web-read (generate-item-cost-prompt (first item-list)))
         (tally (rest item-list)))))

; tally/k : (listof string) (number -> answer) -> answer
(define (tally/k item-list k)
  (if (empty? item-list)
      (k 0)
      (web-read/k (generate-item-cost-prompt (first item-list))
                  (lambda (item-cost)
                    (tally/k (rest item-list)
                             (lambda (total-so-far)
                               (k (+ item-cost
                                     total-so-far))))))))

(tally/k (list "Big Mac" "Le Mac")
         (lambda (x) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-s&h-prompt t) (format "S&H for ~a:" t))

(define (total+s&h item-list)
  (tally/k item-list
           (lambda (return-value)
             (local [(define total return-value)]
               (web-read/k (generate-s&h-prompt total)
                           (lambda (s&h)
                             (+ s&h total)))))))

; (f v1 .. vm) => (f/k v1 ... vm k)

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
