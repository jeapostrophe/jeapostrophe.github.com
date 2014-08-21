#lang racket
;; The library ....
(define (show fmt a) 
  (printf fmt a)
  (eprintf "HTTP Connection closed!"))
(define (prompt p)
  (display p) (read))

(define resumer (box #f))
(define (prompt-with-stuff-after p after-fun)
  (display p)
  (set-box! resumer after-fun)
  (eprintf "HTTP Connection closed!"))
(define (resume ans)
  ((unbox resumer) ans))

;; Our CPS'd program
#|
(define (where-we-start)
  (prompt-with-stuff-after
   "How many numbers?"
   (λ (how-many)
     (get-n-numbers-and-add-them/k 
      how-many
      show-the-sum))))

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
                          rest))))))
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
|#

;; CPSing
(require (for-syntax syntax/parse))
(define-syntax (cps stx)
  (syntax-parse
   stx
   #:literals (if λ prompt let zero? empty? first rest sub1 + cons show)
   [(cps (let ([x e]) body))
    (syntax (cps ((λ (x) body) e)))]
   [(cps x:id) (syntax (λ (k) (k x)))]
   [(cps x:number) (syntax (λ (k) (k x)))]
   [(cps x:str) (syntax (λ (k) (k x)))]
   [(cps (if e0 e1 e2))
    (syntax
     (λ (k)
       ((cps e0)
        (λ (v0)
          (if v0
              ((cps e1) k)
              ((cps e2) k))))))]
   [(cps (λ (x ...) e))
    (syntax
     (λ (k-at-defn)
       (k-at-defn
        (λ (x ... k-at-call)
          ((cps e) k-at-call)))))]
   [(cps (prompt str))
    (syntax
     (λ (k)
       (prompt-with-stuff-after str k)))]
   ; We need to put in the transformations of calls to every primitive function
   [(cps (zero? e))
    (syntax (λ (k) ((cps e) (λ (v) (k (zero? v))))))]
   [(cps (empty? e))
    (syntax (λ (k) ((cps e) (λ (v) (k (empty? v))))))]
   [(cps (first e))
    (syntax (λ (k) ((cps e) (λ (v) (k (first v))))))]
   [(cps (rest e))
    (syntax (λ (k) ((cps e) (λ (v) (k (rest v))))))]
   [(cps (sub1 e))
    (syntax (λ (k) ((cps e) (λ (v) (k (sub1 v))))))]
   [(cps (+ e0 e1))
    (syntax (λ (k) ((cps e0) (λ (v0)
                               ((cps e1) (λ (v1)
                                           (k (+ v0 v1))))))))]
   [(cps (cons e0 e1))
    (syntax (λ (k) ((cps e0) (λ (v0)
                               ((cps e1) (λ (v1)
                                           (k (cons v0 v1))))))))]
   [(cps (show e0 e1))
    (syntax (λ (k) ((cps e0) (λ (v0)
                               ((cps e1) (λ (v1)
                                           (k (show v0 v1))))))))]
   ; Before handling other function calls
   [(cps (f))
    (syntax
     (λ (k)           
       ((cps f)
        (λ (f/k)
          (f/k k)))))]
   [(cps (f e))
    (syntax
     (λ (k)            ; e could be (prompt "Which number?")
       ((cps f)
        (λ (f/k)
          ((cps e)
           (λ (v)
             (f/k v k)))))))]
   [(cps (f e0 e1))
    (syntax
     (λ (k)            ; e could be (prompt "Which number?")
       ((cps f)
        (λ (f/k)
          ((cps e0)
           (λ (v0)
             ((cps e1)
              (λ (v1)
                (f/k v0 v1 k)))))))))]
   [(cps (f e0 e1 e2))
    (syntax
     (λ (k)            ; e could be (prompt "Which number?")
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
  (cps-define (fun-name x ...)
              e)
  (define fun-name
    ((cps (λ (x ...) e)) (λ (c) c))))
(define-syntax-rule (cps-run e)
  ((cps e) (λ (x) x)))

;; Our original program
(cps-define (foldr cons-f empty-v l)
            (if (empty? l)
                empty-v
                (cons-f (first l) (foldr cons-f empty-v (rest l)))))
(cps-define (build-list n f)
            (if (zero? n)
                empty
                (cons (f n) (build-list (sub1 n) f))))
(cps-define (where-we-start)
            (let ([how-many (prompt "How many numbers?")])
              (show "The sum of your numbers is ~a."
                    (foldr ; We can't pass primitives as if they are values, because
                           ; they won't be cps'd
                     (λ (a b) (+ a b)) 0
                     (build-list how-many
                                 (λ (i) (prompt "Number:")))))))
(cps-run (where-we-start))