#lang plai

;; YESTERDAY
;; - Web style:
;; -- functions do not return
;; -- a global transformation (every expression is touched)
;; -- order of evaluation is fixed

;; TODAY
;; - Web style is called CPS

;; 1960s -- continuation passing style or CPS

;; ----> a Stack is a representation (or implementation) of a Continuation

;; This shows up in all async. programs, such as AJAX (flashy Web 2.0) and network servers/operating systems

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

;; (add-two-numbers.com/clo)

;; cps : source code in "direct" style
;;    -> source code in CPS
(require (for-syntax syntax/parse))
(define-syntax (cps stx)
  (syntax-parse stx
    #:literals (displayln + - prompt if)
    [(cps (if cond-e true-e false-e))
     (syntax
       (λ (k)
         ((cps cond-e)
          (λ (cond-v)
            ((if cond-v
               (cps true-e)
               (cps false-e))
             k)))))]
    [(cps some-string:str)
     (syntax
       (λ (k-that-wants-a-string)
         (k-that-wants-a-string some-string)))]
    [(cps (fun-expr value-to-display-expr))
     (syntax
       (λ (k)
         ((cps fun-expr)
          (λ (fun-val)
            ((cps value-to-display-expr)
             (λ (value-to-display)
               (fun-val
                value-to-display
                k)))))))]
    [(cps displayln)
     (syntax
       (λ (k)
         (k web-displayln/clo)))]
    [(cps prompt)
     (syntax
       (λ (k)
         (k web-prompt/clo)))]
    [(cps +)
     (syntax
       (λ (k-that-wants-the-fun)
         (k-that-wants-the-fun
          (λ (lhs-v rhs-v k-that-wants-the-answer)
            (k-that-wants-the-answer
             (+ lhs-v rhs-v))))))]
    [(cps -)
     (syntax
       (λ (k-that-wants-the-fun)
         (k-that-wants-the-fun
          (λ (lhs-v rhs-v k-that-wants-the-answer)
            (k-that-wants-the-answer
             (- lhs-v rhs-v))))))]
    [(cps (fun-expr lhs-expr rhs-expr))
     (syntax
       (λ (k)
         ((cps fun-expr)
          (λ (fun-val)
            ((cps lhs-expr)
             (λ (lhs-value)
               ((cps rhs-expr)
                (λ (rhs-value)
                  (fun-val lhs-value rhs-value k)))))))))]))

;; x -> (λ (k) (k x))

(define (cps-go cpsd-code)
  (cpsd-code (λ (x) x)))


;; (define (add-two-numbers.com/clo-auto)
;;   (cps
;;    (displayln
;;     (+ (prompt "First number:")
;;        (+ (prompt "Second number:")
;;           (prompt "Third number:"))))))

(define (add-two-numbers.com/clo-auto)
  (cps
   (displayln
    ((if (prompt "Add?")
       +
       -)
     (prompt "First number:")
     (prompt "Second number:")))))

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
        ;; (f arg ...) -> (f/clo arg ... (λ (ans) ...))
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
