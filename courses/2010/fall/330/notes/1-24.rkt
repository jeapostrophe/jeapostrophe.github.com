#lang racket
;; The library ....
(define (show fmt a) 
  (printf fmt a)
  (error 'http "Connection closed!"))
(define (prompt p)
  (display p) (read))

(define resumer (box #f))
(define (prompt-with-stuff-after p after-fun)
  (display p)
  (set-box! resumer after-fun)
  (error 'http "Connection closed!"))
(define (resume ans)
  ((unbox resumer) ans))

;; Our program
(define (where-we-start)
  (prompt-with-stuff-after
   "How many numbers?"
   (λ (how-many)
     (get-n-numbers-and-add-them/k 
      how-many
      show-the-sum))))

; This doesn't use higher order functions and so its structure
; is opaque
#;(define (get-n-numbers-and-add-them/k n k)
  (if (zero? n)
      (k 0)      
      (prompt-with-stuff-after 
       "Number:"
       (λ (nth-number)
         (get-n-numbers-and-add-them/k 
          (sub1 n)
          (λ (sum-of-the-others-number)
            (k (+ nth-number
                  sum-of-the-others-number))))))))
; It would really be...
#;(foldr + 0
         (build-list how-many
                     (λ (i) (prompt "Number:"))))
; in direct style
; But this is it in CPS (continuation passing style)
(define (foldr/k cons empty l k)
  (if (empty? l)
      (k empty)
      (foldr/k cons empty (rest l)
               (λ (rest)
                 (k (cons (first l)
                          rest))))))                         ; STACK
(define (build-list/k n f/k k)
  (if (zero? n) 
      (k empty)
      (f/k n 
           (λ (nth-entry)
             (build-list/k (sub1 n) f/k
                           (λ (other-entries)
                             (k (cons nth-entry
                                      other-entries))))))))
(define (get-n-numbers-and-add-them/k n k)
  (build-list/k n
                (λ (i k) 
                  (prompt-with-stuff-after "Number:" k))
                (λ (l)
                  (foldr/k + 0 l k))))

(define (show-the-sum the-sum)
  (show "The sum of your numbers is ~a."
        the-sum))


#;(get-n-numbers-and-add-them/k 
   2
   show-the-sum)
; =>
#;(if (zero? 2)
      (k 0)      
      (prompt-with-stuff-after 
       "Number:"
       (λ (nth-number)
         (get-n-numbers-and-add-them/k 
          (sub1 2)
          (λ (sum-of-the-others-number)
            (show-the-sum (+ nth-number
                             sum-of-the-others-number)))))))
; =>
#;(prompt-with-stuff-after 
   "Number:"
   (λ (nth-number)
     (get-n-numbers-and-add-them/k 
      1
      (λ (sum-of-the-others-number)
        (show-the-sum (+ nth-number
                         sum-of-the-others-number))))))
; =>
#;(prompt-with-stuff-after 
   "Number:"
   (λ (nth-number0)
     (if (zero? 1)
         (k 0)      
         (prompt-with-stuff-after 
          "Number:"
          (λ (nth-number1)
            (get-n-numbers-and-add-them/k 
             (sub1 1)
             (λ (sum-of-the-others-number1)
               ((λ (sum-of-the-others-number0)
                  (show-the-sum (+ nth-number0
                                   sum-of-the-others-number0)))
                (+ nth-number1
                   sum-of-the-others-number1)))))))))
; =>
#;(prompt-with-stuff-after 
   "Number:"
   (λ (nth-number0)
     (prompt-with-stuff-after 
      "Number:"
      (λ (nth-number1)
        (get-n-numbers-and-add-them/k 
         0
         (λ (sum-of-the-others-number1)
           ((λ (sum-of-the-others-number0)
              (show-the-sum (+ nth-number0
                               sum-of-the-others-number0)))
            (+ nth-number1
               sum-of-the-others-number1))))))))
; =>
#;(prompt-with-stuff-after 
   "Number:"
   (λ (nth-number0)
     (prompt-with-stuff-after 
      "Number:"
      (λ (nth-number1)
        ((λ (sum-of-the-others-number1)
           ((λ (sum-of-the-others-number0)
              (show-the-sum (+ nth-number0
                               sum-of-the-others-number0)))
            (+ nth-number1
               sum-of-the-others-number1)))
         0)))))

(where-we-start)