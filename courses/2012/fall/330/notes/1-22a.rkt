#lang plai

;; YESTERDAY
;; - Web style:
;; -- functions do not return
;; -- a global transformation (every expression is touched)
;; -- order of evaluation is fixed

;; TODAY
;; - Web style is called CPS
;; - CPS can be automated
;; -- applications
;; -- primitives
;; -- Web primitives
;; -- ifs
;; -- lets
;; -- lambdas

;; CONSOLE
(define (prompt text)
  (displayln text)
  (read))
(define (add-two-numbers.exe)
  (displayln
   (+ (prompt "First number:")
      (prompt "Second number:"))))

;; WEB w/ closures
(define (web-prompt/clo text clo)
  (web-displayln/clo text clo))
(define (web-displayln/clo text clo)
  (printf "<html><input type=\"hidden\" value=\"~a\"/>\n~a</html>\n" clo text)
  (clo (read))
  (error 'http "I'm stateless"))

(define (add-two-numbers.com/clo)
  (web-prompt/clo
   "First number:"
   (λ (first-number)
     (web-prompt/clo
      "Second number:"
      (λ (second-number)
        (web-displayln/clo
         (+ first-number
            second-number)
         void))))))

;; WEB w/ closures - automatically

(require (for-syntax syntax/parse))
(define-syntax (cps stx)
  (syntax-parse stx
    #:literals (displayln prompt + - let λ)
    [(cps (λ (param:id) body-e))
     (syntax
       (λ (static-k)
         (static-k
          (λ (param dyn-k)
            ((cps body-e)
             dyn-k)))))]
    [(cps (let ([x:id x-e]) body-e))
     (syntax (cps ((λ (x) body-e) x-e)))]
    [(cps s:str)
     (syntax
       (λ (k)
         (k s)))]
    [(cps (if cond-e true-e false-e))
     (syntax
       (λ (k)
         ((cps cond-e)
          (λ (cond-v)
            ((if cond-v
               (cps true-e)
               (cps false-e))
             k)))))]
    [(cps prompt)
     (syntax
       (λ (k)
         (k web-prompt/clo)))]
    [(cps displayln)
     (syntax
       (λ (k)
         (k web-displayln/clo)))]
    [(cps +)
     (syntax
       (λ (k)
         (k (λ (lhs rhs k)
              (k (+ lhs rhs))))))]
    [(cps -)
     (syntax
       (λ (k)
         (k (λ (lhs rhs k)
              (k (- lhs rhs))))))]
    [(cps x:id)
     (syntax
       (λ (k)
         (k x)))]
    [(cps (fun arg))
     (syntax
       (λ (k)
         ((cps fun)
          (λ (fun-v)
            ((cps arg)
             (λ (arg-v)
               (fun-v arg-v k)))))))]
    [(cps (fun arg1 arg2))
     (syntax
       (λ (k)
         ((cps fun)
          (λ (fun-v)
            ((cps arg1)
             (λ (arg1-v)
               ((cps arg2)
                (λ (arg2-v)
                  (fun-v arg1-v arg2-v k)))))))))]))

(define (cps-go e)
  (e (λ (x) x)))

(define (add-two-numbers.com/clo-auto)
  (cps
   (let ([fun
          (if (prompt "Add?")
            + -)])
     (displayln
      (fun
       (prompt "First number:")
       (prompt "Second number:"))))))

(cps-go (add-two-numbers.com/clo-auto))

(define (sum l)
  (foldr + 0 l))

(define (build-list n f)
  (cond
    [(zero? n)
     empty]
    [else
     (define the-rest
       (build-list (sub1 n) f))
     (cons (f n)
           the-rest)]))
(define (add-many-numbers.exe)
  (define how-many
    (prompt "How many numbers?"))
  (displayln
   (format
    "Your ~a numbers sum to ~a"
    how-many
    (sum
     (build-list
      how-many
      (λ (i)
        (prompt (format "~ath number?" i))))))))

;; (add-many-numbers.exe)

(define (web-build-list/clo n web-f/clo clo)
  (cond
    [(zero? n)
     (clo empty)]
    [else
     (web-build-list/clo
      (sub1 n) web-f/clo
      (λ (the-rest)
        (web-f/clo
         n
         (λ (the-first)
           (clo
            (cons the-first
                  the-rest))))))]))
(define (add-many-numbers.com/clo)
  (web-prompt/clo
   "How many numbers?"
   (λ (how-many)
     (web-build-list/clo
      how-many
      (λ (i clo)
        (web-prompt/clo
         (format "~ath number?" i)
         clo))
      (λ (the-numbers)
        (web-displayln/clo
         (format
          "Your ~a numbers sum to ~a"
          how-many
          (sum
           the-numbers))
         void))))))

;; (add-many-numbers.com/clo)
