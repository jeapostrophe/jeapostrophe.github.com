;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-5) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
; check-temps1 : lon -> boolean
; Purpose: To determine if a every number is between 5 and 95
(define (check-temps1 lon)
  (cond [(empty? lon) true]
        [(cons? lon)
         (and (<= 5 (first lon) 95)
              (check-temps1 (rest lon)))]))

(check-expect (check-temps1 empty) true)
(check-expect (check-temps1 (list 60)) true)
(check-expect (check-temps1 (list 3)) false)
(check-expect (check-temps1 (list 100)) false)

; check-temps : lon num num -> boolean
; Purpose: To determine if a every number is between lo and hi
(define (check-temps lon lo hi)
  (cond [(empty? lon) true]
        [(cons? lon)
         (and (<= lo (first lon) hi)
              (check-temps (rest lon) lo hi))]))

(check-expect (check-temps empty 5 95) true)
(check-expect (check-temps (list 60) 5 95) true)
(check-expect (check-temps (list 3) 5 95) false)
(check-expect (check-temps (list 100) 5 95) false)

; convert : lon -> num
; Purpose: To compute the number represented by the digits in less significant order
(define (convert lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         (+ (first lon)
            (* 10 (convert (rest lon))))]))

(check-expect (convert empty) 0)
(check-expect (convert (list 3)) 3)
(check-expect (convert (list 3 2 1)) 123)

; average-price : lon -> num
; Purpose: To compute the average of the list
(define (average-price lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         
         ; \Sigma x_i {i=0,n} / n
         
         ; x_n / n
         ; \Sigma x_i {i=0,n-1} / n - 1 * ((n - 1) / n)
         
         (+ (/ (first lon)
               (length lon)) 
            (* (average-price (rest lon))
               (/ (length (rest lon))
                  (length lon))))]))

(check-expect (average-price empty) 0)
(check-expect (average-price (list 1)) 1)
(check-expect (average-price (list 1 1)) 1)
(check-expect (average-price (list 1 2 3)) 2)

; convertFC : lon -> lon
; Purpose: Convert a cooking book from American to Wrong
(define (convertFC lon)
  (cond [(empty? lon) empty]
        [(cons? lon)
         (cons (* 5/9 (- (first lon) 32))
               (convertFC (rest lon)))]))

(check-expect (convertFC empty) empty)
(check-expect (convertFC (list 32)) (list 0))
(check-expect (convertFC (list -40)) (list -40))
(check-expect (convertFC (list 212)) (list 100))

; eliminate-exp : num lon -> lon
; Purpose: Budget Jay's spending on toys
(define (eliminate-exp limit lon)
  (cond [(empty? lon) empty]
        [(cons? lon)
         (cond [(<= (first lon) limit)
                (cons (first lon) (eliminate-exp limit (rest lon)))]
               [else
                (eliminate-exp limit (rest lon))])]))

(check-expect (eliminate-exp pi empty) empty)
(check-expect (eliminate-exp pi (list 1)) (list 1))
(check-expect (eliminate-exp pi (list 1 5)) (list 1))
(check-expect (eliminate-exp pi (list 5)) empty)

; suffixes : list -> listof list
; Purpose: To confuse 330 students
(define (suffixes l)
  (cond [(empty? l)
         (list empty)]
        [(cons? l)
         (cons l
               (suffixes (rest l)))]))

(check-expect (suffixes empty) (list empty))
(check-expect (suffixes (list 'a 'b 'c 'd)) 
              (list (list 'a 'b 'c 'd) (list 'b 'c 'd) (list 'c 'd) (list 'd) empty))

; A family-tree is either
; - an unknown
; - a person

; An unknown is a (make-unknown)
(define-struct unknown ())

; A person is a (make-person name birthyear eyecolor father mother)
; - a name is a string
; - a birthyear is a number
; - an eyecolor is a symbol
; - a father is a family-tree
; - a mother is a family-tree
(define-struct person (name birthyear eyecolor father mother))

; count-persons : family-tree -> number
; Purpose: To compute the number of "person"s
(define (count-persons ft)
  (cond [(unknown? ft) 0]
        [(person? ft)
         (+ 1 
            (count-persons (person-father ft))
            (count-persons (person-mother ft)))]))

(check-expect (count-persons (make-unknown)) 0)
(check-expect (count-persons (make-person "Jay" 1985 'blue (make-unknown) (make-unknown))) 1)
(check-expect (count-persons 
               (make-person "Jay" 1985 'blue
                            (make-person "Jim" 1900 'magenta (make-unknown) (make-unknown))
                            (make-unknown))) 2)
(check-expect (count-persons 
               (make-person "Jay" 1985 'blue
                            (make-person "Jim" 1900 'magenta (make-unknown) (make-unknown))
                            (make-person "Pam" 1950 'magenta (make-unknown) (make-unknown))))
              3)

; average-age : family-tree -> number
; Purpose: To compute the average age assuming current year is 2010
(define (average-age ft)
  (cond [(unknown? ft) 0]
        [(person? ft)
         (+
          (/ (- 2010 (person-birthyear ft))
             (count-persons ft))
          
          (* (average-age (person-father ft))
             (/ (count-persons (person-father ft))
                (count-persons ft)))
          
          (* (average-age (person-mother ft))
             (/ (count-persons (person-mother ft))
                (count-persons ft))))]))

(check-expect (average-age (make-unknown))
              0)
(check-expect (average-age (make-person "Jay" 1985 'blue (make-unknown) (make-unknown)))
              25)
(check-expect (average-age 
               (make-person "Jay" 1985 'blue
                            (make-person "Jim" 1900 'magenta (make-unknown) (make-unknown))
                            (make-unknown)))
              (/ (+ 25 110) 2))
(check-expect (average-age 
               (make-person "Jay" 1985 'blue
                            (make-person "Jim" 1900 'magenta (make-unknown) (make-unknown))
                            (make-person "Pam" 1950 'magenta (make-unknown) (make-unknown))))
              (/ (+ 25 110 60) 3))

; eye-colors : family-tree -> list-of-symbols
; Purpose: To compute the eye colors of everyone in the family
(define (eye-colors ft)
  (cond [(unknown? ft) empty]
        [(person? ft)
         (cons
          (person-eyecolor ft)
          (append (eye-colors (person-father ft))
                  (eye-colors (person-mother ft))))]))
         
(check-expect (eye-colors (make-unknown))
              empty)
(check-expect (eye-colors (make-person "Jay" 1985 'blue (make-unknown) (make-unknown)))
              (list 'blue))
(check-expect (eye-colors 
               (make-person "Jay" 1985 'blue
                            (make-person "Jim" 1900 'magenta (make-unknown) (make-unknown))
                            (make-unknown)))
              (list 'blue 'magenta))
(check-expect (eye-colors 
               (make-person "Jay" 1985 'blue
                            (make-person "Jim" 1900 'magenta (make-unknown) (make-unknown))
                            (make-person "Pam" 1950 'brown (make-unknown) (make-unknown))))
              (list 'blue 'magenta 'brown))

; my-append : list list -> list
; Purpose: To create a list with all the elements from the first and all the elements from the second list
(define (my-append before after)
  (cond [(empty? before) after]
        [(cons? before)
         (cons (first before)
               (my-append (rest before) after))]))
