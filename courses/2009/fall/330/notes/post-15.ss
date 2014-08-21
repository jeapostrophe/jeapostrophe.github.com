;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
; add-two-numbers.com
(define (add-two-numbers)
  (web-display
   (+ (web-read "First number")
      (web-read "Second number"))))

; web-read/next : string (number -> html) -> html

(define (add-two-numbers/next)
  (web-read/next
   "First number"
   (lambda (first-number)
     (web-read/next
      "Second number"
      (lambda (second-number)
        (web-display (+ first-number second-number)))))))

; web-read/next-name : string string (listof string) -> html

(define (add-two-numbers/next-name)
  (web-read/next-name
   "First number"
   "got-the-first-number"
   empty))

(define (got-the-first-number env)
  (web-read/next-name
   "Second number"
   "got-the-second-number"
   (list (list 'first (extract 'input env)))))

(define (got-the-second-number env)
  (local [(define first-number (extract 'first env))
          (define second-number (extract 'input env))]
    (web-display (+ first-number second-number))))

