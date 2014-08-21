#lang racket
(define current-browser-tab (box "google.com"))
(define (prompt/k p after-fun)
  (display p)
  (set-box! current-browser-tab after-fun)
  (eprintf "HTTP Connection closed!"))
(define (submit n)
  ((unbox current-browser-tab) n))

(define (show fmt a)
  (printf fmt a)
  (eprintf "HTTP Connection closed!"))
(define (prompt p)
  (display p) (read))

#;(define (start)
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
    #;(let ([how-many (prompt "How may numbers?")])
        (show "The sum of your numbers is ~a."
              (get-n-numbers-and-add-them how-many)))
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

;; CPSing
(require (for-syntax syntax/parse))
(define-syntax (cps stx)
  (syntax-parse
   stx
   #:literals (λ show cons first rest empty? zero? sub1 + let if
                ; JAY IS DUMB! FORGOT TO ADD 'prompt' as primitive
                prompt)
   [(cps x:id)
    (syntax (λ (k) (k x)))]
   [(cps x:number)
    (syntax (λ (k) (k x)))]
   [(cps x:str)
    (syntax (λ (k) (k x)))]
   [(cps (λ (x ...) body))
    (syntax
     (λ (k-at-defn)
       (k-at-defn
        (λ (x ... k-at-call)
          ((cps body) k-at-call)))))]
   [(cps (prompt str))
    (syntax
     ; It is all for this line:!!!!
     (λ (k) ((cps str) (λ (strv) (prompt/k strv k)))))]
   [(cps (show fmt a))
    (syntax
     (λ (k) ((cps fmt) (λ (fmtv) ((cps a) (λ (av) (k (show fmtv av))))))))]
   [(cps (cons fmt a))
    (syntax
     (λ (k) ((cps fmt) (λ (fmtv) ((cps a) (λ (av) (k (cons fmtv av))))))))]
   [(cps (+ fmt a))
    (syntax (λ (k) ((cps fmt) (λ (fmtv) ((cps a) (λ (av) (k (+ fmtv av))))))))]
   [(cps (zero? a))
    (syntax
     (λ (k) ((cps a) (λ (av) (k (zero? av))))))]
   [(cps (empty? a))
    (syntax
     (λ (k) ((cps a) (λ (av) (k (empty? av))))))]
   [(cps (first a))
    (syntax
     (λ (k) ((cps a) (λ (av) (k (first av))))))]
   [(cps (rest a))
    (syntax
     (λ (k) ((cps a) (λ (av) (k (rest av))))))]
   [(cps (sub1 a))
    (syntax
     (λ (k) ((cps a) (λ (av) (k (sub1 av))))))]
   [(cps (if test true false))
    (syntax
     (λ (k) 
       ((cps test)
        (λ (test_v)
          (if test_v
              ((cps true) k)
              ((cps false) k))))))]
   [(cps (let ([x e]) body))
    (syntax
     (cps ((λ (x) body) e)))]
   [(cps (f))
    (syntax 
     (λ (k)
       ((cps f)
        (λ (f/k)
          (f/k k)))))]
   [(cps (f e0))
    (syntax
     (λ (k)
       ((cps f)
        (λ (f/k)
          ((cps e0)
           (λ (v0)
             (f/k v0 k)))))))]
   [(cps (f e0 e1))
    (syntax
     (λ (k)
       ((cps f)
        (λ (f/k)
          ((cps e0)
           (λ (v0)
             ((cps e1)
              (λ (v1)
                (f/k v0 v1 k)))))))))]
   [(cps (f e0 e1 e2))
    (syntax
     (λ (k)
       ((cps f)
        (λ (f/k)
          ((cps e0)
           (λ (v0)
             ((cps e1)
              (λ (v1)
                ((cps e2)
                 (λ (v2)
                   (f/k v0 v1 v2 k)))))))))))]))

(define-syntax-rule
  (cps-this-program!
   (define (fun-name x ...)
     body)
   ...
   final-e)
  (begin
    (define fun-name
      ((cps (λ (x ...) body))
       (λ (v) v)))
    ...
    ((cps final-e) display)))

;; Our program

(cps-this-program!
 (define (foldr f-cons v-empty l)
   (if (empty? l)
       v-empty
       (f-cons (first l)
               (foldr f-cons v-empty (rest l)))))
 (define (build-list n f)
   (if (zero? n)
       empty
       (cons (f n) (build-list (sub1 n) f))))
 (define (get-a-number i)
   (prompt "Number:"))
 (define (get-n-numbers-and-add-them n)
   (foldr (λ (a b) (+ a b)) 0 (build-list n get-a-number)))
 (let ([how-many (prompt "How may numbers?")])
   (show "The sum of your numbers is ~a."
         (get-n-numbers-and-add-them how-many))))