;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
; Testing Web Transformations
(define the-receiver (box 'dummy))
(define receiver-prompt (box 'dummy))

(define (web-display n k)
  (printf "Web output: ~a~n" n)
  (k (void)))

(define (web-read p) (error 'web-read "Can't web read!"))
(define (web-read/k p k)
  (begin (set-box! receiver-prompt p)
         (set-box! the-receiver k)
         (error 'web-read/k "Run (resume) to enter number and simulate clicking Submit")))

(define (resume)
  (begin (display (unbox receiver-prompt))
         ((unbox the-receiver) (read))))

; add-two-numbers.com
(define (add-two-numbers)
  (web-display
   (+ (web-read "First number")
      (web-read "Second number"))))

; web-read/k: string (number -> html) -> html

(define (add-two-numbers/k)
  (web-read/k
   "First number"
   (lambda (first-number)
     (web-read/k
      "Second number"
      (lambda (second-number)
        (web-display (+ first-number second-number)))))))

; (cps e) : function that takes one argument, the continuation, and calls it in tail-position with its result and all sub-expressions in the result are in cps

; 1 => (lambda (k) (k 1))
; x => (lambda (k) (k x))
; (lambda (x) x) => (lambda (k) (k (lambda (x dyn-k) (dyn-k x))))

(define-syntax cps
  (syntax-rules (+ lambda web-read)
    [(cps (+ e1 e2))
     (lambda (k)
       ((cps e1) 
        (lambda (e1-val)
          ((cps e2) 
           (lambda (e2-val)
             (k (+ e1-val e2-val)))))))]
    [(cps (web-read prompt))
     (lambda (k)
       (web-read/k prompt k))]
    [(cps (f v))
     (lambda (k)
       ((cps f)
        (lambda (f-val)
          ((cps v)
           (lambda (v-val)
             (f-val v-val k))))))]
    [(cps (lambda (a) body))
     (lambda (k)
       (k (lambda (a dyn-k)
            ((cps body) dyn-k))))]
    [(cps v)
     (lambda (k)
       (k v))]))

(define (add-two-numbers/cps k)
  ((cps (web-display
         (+ (web-read "First number")
            (web-read "Second number"))))
   k))

; define-cps
(define-syntax define-cps
  (syntax-rules ()
    [(define-cps (f arg) body)
     (define-cps f (lambda (arg) body))]
    [(define-cps v val)
     (define v ((cps val) (lambda (x) x)))]))

; run
(define-syntax run
  (syntax-rules ()
    [(run e)
     ((cps e)
      (lambda (x)
        (error 'run "Terminating with value: ~a" x)))]))
