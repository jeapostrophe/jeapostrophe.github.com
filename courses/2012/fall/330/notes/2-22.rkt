#lang plai

;; YESTERDAY
;; - Web style:
;; -- functions do not return
;; -- a global transformation (every expression is touched)
;; -- order of evaluation is fixed

;; TODAY
;; - Web style is called CPS

;; 1960s... "continuation passing style" (CPS)

;; A Hash Table ... an implementation the data-structure "Map"

;; A Stack... is an impl. tech. for a Continuation

;; A stack is "what I will do next"... a continuation is also this

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

;; cps : syntax in "direct-style"
;;   -> syntax in CPS
(require (for-syntax syntax/parse))
(define-syntax (cps stx)
  (syntax-parse stx
    #:literals (displayln + prompt)
    [(cps (+ lhs-expr rhs-expr))
     (syntax
       (λ (k-that-wants-the-result-of-+)
         ((cps lhs-expr)
          (λ (lhs-value)
            ((cps rhs-expr)
             (λ (rhs-value)
               (k-that-wants-the-result-of-+
                (+ lhs-value rhs-value))))))))]
    [(cps (displayln expr-to-display))
     (syntax
       (λ (k-that-wants-the-result-of-displayln)
         ((cps expr-to-display)
          (λ (value-to-display)
            (web-displayln/clo
             value-to-display
             k-that-wants-the-result-of-displayln)))))]
    [(cps (prompt expr-to-display))
     (syntax
       (λ (k-that-wants-the-result-of-prompt)
         ((cps expr-to-display)
          (λ (value-to-display)
            (web-prompt/clo
             value-to-display
             k-that-wants-the-result-of-prompt)))))]
    [(cps x:id)
     (syntax
       (λ (k-that-wants-an-id)
         (k-that-wants-an-id x)))]
    [(cps x:str)
     (syntax
       (λ (k-that-wants-an-string)
         (k-that-wants-an-string x)))]))

;; (cps x) ===> (λ (k-that-wants-an-id)
;;               (k-that-wants-an-id x)

(define (cps-go cpsd-code)
  (cpsd-code (λ (x) x)))

(define (add-two-numbers.com/clo-auto)
  (cps
   (displayln
    (+ (prompt "First number:")
       (+ (prompt "Second number:")
          (prompt "Third number:"))))))

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
