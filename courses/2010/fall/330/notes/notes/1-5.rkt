;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1-5) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
; check-temps1 : lon -> boolean
; Purpose: Ensure that temps are between 5 and 95
(define (check-temps1 lon)
  (cond [(empty? lon) true]
        [(cons? lon) 
         (and (<= 5 (first lon) 95)
              (check-temps1 (rest lon)))]))

(check-expect (check-temps1 empty) true)
(check-expect (check-temps1 (list 1 60)) false)
(check-expect (check-temps1 (list 60)) true)

; check-temps : lon num num -> boolean
; Purpose: Ensure that temps are between lo and hi
(define (check-temps lon lo hi)
  (cond [(empty? lon) true]
        [(cons? lon) 
         (and (<= lo (first lon) hi)
              (check-temps (rest lon) lo hi))]))

(check-expect (check-temps empty 5 95) true)
(check-expect (check-temps (list 1 60) 5 95) false)
(check-expect (check-temps (list 60) 5 95) true)

; convert : lon -> number
; Purpose: Convert digits in least-significant first order into a number
(define (convert digits)
  (cond [(empty? digits) 0]
        [(cons? digits)
         (+ (first digits)
            (* 10 (convert (rest digits))))]))

(check-expect (convert empty) 0)
(check-expect (convert (list 1 2 3)) 321)

; average-price : lon -> number
; Purpose: Compute the average price
(define (average-price lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         ; \Sigma i / n = i / n + .... n - 1 / n * \Sigma i-1 / n - 1
         (+ (/ (first lon)
               (length lon)) ; n
            (* (/ 
                (length (rest lon)) ; n - 1
                (length lon))
               (average-price (rest lon))))]))

(check-expect (average-price empty) 0)
(check-expect (average-price (list 1)) 1)
(check-expect (average-price (list 1 1)) 1)
(check-expect (average-price (list 1 2 3)) 2)

; convertFC : lon -> lon
; Purpose: Convert every Fahrenheit to Celsius
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
; Purpose: Eliminate toys greater than Jay's wallet
(define (eliminate-exp limit lon)
  (cond [(empty? lon) empty]
        [(cons? lon)
         (cond [(<= (first lon) limit)
                (cons (first lon) (eliminate-exp limit (rest lon)))]
               [else
                (eliminate-exp limit (rest lon))])]))

(check-expect (eliminate-exp pi empty) empty)
(check-expect (eliminate-exp pi (list 5)) empty)
(check-expect (eliminate-exp pi (list 1)) (list 1))
(check-expect (eliminate-exp pi (list 1 5)) (list 1))

; suffixes : list -> listof list
; Purpose: Compute every suffix of the input
(define (suffixes l)
  (cond [(empty? l)
         (list empty)]
        [(cons? l)
         (cons l
               (suffixes (rest l)))]))

(check-expect (suffixes (list 'a 'b 'c 'd))
              (list (list 'a 'b 'c 'd) (list 'b 'c 'd) (list 'c 'd) (list 'd) empty))

; A family tree is either
; - an unknown
; - a person

; An unknown is a (make-unknown)
(define-struct unknown ())

; A person is a (make-person name birthyear eyecolor father mother) where
; - a name is a string
; - a birthyear is a number
; - a eyecolor is a symbol
; - a father is a family-tree
; - a mother is a family-tree
(define-struct person (name birthyear eyecolor father mother))

; count-persons : ftree -> number
; Purpose: To compute how many ancestors are in the tree
(define (count-persons ft)
  (cond [(unknown? ft)
         0]
        [(person? ft) 
         (+ 1 
            (count-persons (person-father ft))
            (count-persons (person-mother ft)))]))

(check-expect (count-persons (make-unknown)) 0)         
(check-expect (count-persons (make-person "Jay" 1985 'blue (make-unknown) (make-unknown))) 1)
(check-expect (count-persons 
               (make-person "Jay" 1985 'blue 
                            (make-unknown)
                            (make-person "Pam" 1950 'brown (make-unknown) (make-unknown))))
              2)

; average-age : ftree -> number
; Purpose: To compute the average age assuming 2010 is the current year
(define (average-age ft)
  (cond [(unknown? ft)
         0]
        [(person? ft)
         
         ; n = mn + fn + 1
         ; \Sigma x_i / n = x_n ... 
         ; ... fn / mn + fn + 1 * \Sigma x_i {i = fs...fe} / fn ... 
         ; ... \Sigma x_i {i = ms ... me} / mn 
         
         (+
          (/ (- 2010 (person-birthyear ft))
             (count-persons ft))
          
          (* (average-age (person-father ft))
             (/ (count-persons (person-father ft))
                (count-persons ft)))
          
          (* (average-age (person-mother ft))
             (/ (count-persons (person-mother ft))
                (count-persons ft))))]))

(check-expect (average-age (make-unknown)) 0)
(check-expect (average-age (make-person "Jay" 1985 'blue (make-unknown) (make-unknown)))
              25)
(check-expect (average-age 
               (make-person "Jay" 1985 'blue
                            (make-unknown)
                            (make-person "Pam" 1950 'brown (make-unknown) (make-unknown))))
              (/ (+ 25 60) 2))
(check-expect (average-age 
               (make-person "Jay" 1985 'blue
                            (make-person "Jim" 1950 'brown (make-unknown) (make-unknown))
                            (make-person "Pam" 1950 'brown (make-unknown) (make-unknown))))
              (/ (+ 25 60 60) 3))

; eye-colors : ftree -> list-of-symbols
; Purpose: Compute every eye color in the family
(define (eye-colors ft)
  (cond [(unknown? ft) empty]
        [(person? ft)
         (cons
          (person-eyecolor ft)
          (append (eye-colors (person-father ft))
                  (eye-colors (person-mother ft))))]))

(check-expect (eye-colors (make-unknown)) empty)
(check-expect (eye-colors (make-person "Jay" 1985 'blue (make-unknown) (make-unknown)))
              (list 'blue))
(check-expect (eye-colors 
               (make-person "Jay" 1985 'blue
                            (make-unknown)
                            (make-person "Pam" 1950 'brown (make-unknown) (make-unknown))))
              (list 'blue 'brown))
(check-expect (eye-colors 
               (make-person "Jay" 1985 'blue
                            (make-person "Jim" 1950 'magenta (make-unknown) (make-unknown))
                            (make-person "Pam" 1950 'brown (make-unknown) (make-unknown))))
              (list 'blue 'magenta 'brown))

