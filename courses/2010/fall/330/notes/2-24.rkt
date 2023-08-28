#lang racket
(define current-browser-tab (box "google.com"))
(define (prompt/k p after-fun)
  (display p)
  (set-box! current-browser-tab after-fun)
  (error 'http "Connection closed!"))
(define (submit n)
  ((unbox current-browser-tab) n))

(define (show fmt a)
  (printf fmt a)
  (error 'http "Connection closed!"))

(define (start)
  ; This is a CONTINUATION
  ; is abbreviated kont
  ; is abbreviated k
  
  ; This is the program in DIRECT style
  #;(define (get-n-numbers-and-add-them n)
      (if (zero? n)
          0
          (+ (get-a-number)
             (get-n-numbers-and-add-them (sub1 n)))))
  ; We have to write in CONTINUATION PASSING STYLE (CPS)
  #;(define (get-n-numbers-and-add-them/k n k)
    (if (zero? n)
        (k 0)
        (prompt/k
         "Number:"
         (λ (nth-number)
           (get-n-numbers-and-add-them/k
            (sub1 n)
            (λ (the-rest-of-the-sum)
              (k (+ nth-number
                    the-rest-of-the-sum))))))))
  
  ; This is the program in DIRECT style
  #;(define (foldr f-cons v-empty l)
      (if (empty? l)
          v-empty
          (f-cons (first l)
                  (foldr f-cons v-empty (rest l)))))
  #;(define (build-list n f)
      (if (zero? n)
          empty
          (cons (f n) (build-list (sub1 n) f))))
  #;(define (get-a-number i)
      (prompt "Number:"))
  #;(define (get-n-numbers-and-add-them n)
      (foldr + 0 (build-list n get-a-number)))
  ; We have to write in CONTINUATION PASSING STYLE (CPS)
  (define (foldr/k f-cons/k v-empty l k)
    (if (empty? l)
        (k v-empty)
        (foldr/k f-cons/k v-empty (rest l)
                 (λ (the-rest-v)
                   (f-cons/k (first l)
                           the-rest-v
                           k
                           #;(λ (the-ans)
                             (k the-ans)))))))
  ; What part of a normal implementation is no longer necessary?
  ; ANSWER: 
  (define (build-list/k n f/k k)
    (if (zero? n)
        (k empty)
        (f/k n
             (λ (nth-entry)
               (build-list/k (sub1 n) f/k
                             (λ (all-the-others)
                               (k (cons nth-entry all-the-others))))))))
  (define (get-n-numbers-and-add-them/k n k)
    (build-list/k
     n 
     (λ (i k)
       (prompt/k "Number:" k))
     (λ (the-numbers)
       ; We don't need to change this, but a program wouldn't know that
       #;(k (foldr + 0 the-numbers))
       (foldr/k (λ (a b k) (k (+ a b))) ; Here we are wrapping a primitive
                0
                the-numbers
                k))))
  
  (prompt/k
   "How many numbers?"
   (λ (how-many)
     (get-n-numbers-and-add-them/k
      how-many
      (λ (the-sum)
        (show "The sum of your numbers is ~a."
              the-sum))))))

(start)