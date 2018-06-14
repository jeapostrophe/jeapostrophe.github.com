#lang racket/base
(require racket/match
         racket/list
         racket/format
         racket/dict)
(module+ test
  (require chk))
(define-syntax-rule
  (δ [(qi read-s) (qj write-s dir)] ...)
  (λ (q* s*)
    (match (cons q* s*)
      [(cons 'qi read-s) (list 'qj write-s 'dir)] ...)))

;; A TM is a start state plus a delta function
(struct TM (q0 δ))

;; abc [qi] def
;; => (config (list c b a) qi (list d e f))
(struct config (left q right))

(define (first* l) (if (empty? l) '☠ (first l)))
(define (rest* l) (if (empty? l) l (rest l)))
(define (cons* x l) (if (and (eq? x '☠) (empty? l)) l (cons x l)))

(define (step δ ci)
  (match-define (config left qi right) ci)
  (match qi
    [#t right]
    [#f #f]
    [qi
     (match-define (list qj new-char dir)
       (δ qi (first* right)))
     (match dir
       ['R (config (cons* new-char left) qj (rest* right))]
       ['L (config (rest* left) qj (cons* (first* left) (cons* new-char (rest* right))))])]))

(define (step* inform! δ ci)
  (inform! ci)
  (define cj (step δ ci))
  (if (config? cj)
    (step* inform! δ cj)
    cj))

(define (initial-config q0 the-input)
  (config empty q0 the-input))

(define (run the-tm the-input #:inform! [inform! void])
  (match-define (TM q0 δ) the-tm)
  (step* inform! δ (initial-config q0 the-input)))

(define (display-config c)
  (match-define (config left qi right) c)
  (displayln (~a (reverse left) "[" qi "]" right)))
(define (show the-tm the-input)
  (run the-tm the-input #:inform! display-config))

(module+ test
  ;; 0^n10^m => 0^n+m
  (define unary-addition
    (TM 'move-right
        (δ [(move-right 0) (move-right 0 R)]
           [(move-right '☠) (#f '☠ R)]
           [(move-right 1) (delete-last-0 0 R)]
           [(delete-last-0 1) (#f '☠ R)]
           [(delete-last-0 0) (delete-last-0 0 R)]
           [(delete-last-0 '☠) (found-last-0 '☠ L)]
           [(found-last-0 0) (move-left '☠ L)]
           [(move-left 0) (move-left 0 L)]
           [(move-left '☠) (#t '☠ R)])))

  (chk (run unary-addition '(0 0 1 0 0 0)) '(0 0 0 0 0))
  (show unary-addition '(0 0 1 0 0 0))

  (define binary-add1
    (TM 'find-end
        (δ [(find-end 0) (find-end 0 R)]
           [(find-end 1) (find-end 1 R)]
           [(find-end '☠) (zero-until-0 '☠ L)]
           [(zero-until-0 1) (zero-until-0 0 L)]
           [(zero-until-0 0) (go-left 1 L)]
           [(zero-until-0 '☠) (go-back-right-once 1 L)]
           [(go-back-right-once '☠) (#t '☠ R)]
           [(go-left 0) (go-left 0 L)]
           [(go-left 1) (go-left 1 L)]
           [(go-left '☠) (#t '☠ R)])))
  (show binary-add1 '(0 1 0 0 0 1 1 0))

  (define binary-sub1
    (TM 'ones-complement1
        (δ [(ones-complement1 1)  (ones-complement1 0 R)]
           [(ones-complement1 0)  (ones-complement1 1 R)]
           [(ones-complement1 '☠) (do-addition '☠ L)]
           [(do-addition 0) (do-addition 0 L)]
           [(do-addition 1) (do-addition 1 L)]
           [(do-addition '☠) (find-end '☠ R)]
           ;; This is binary-add1
           [(find-end 0) (find-end 0 R)]
           [(find-end 1) (find-end 1 R)]
           [(find-end '☠) (zero-until-0 '☠ L)]
           [(zero-until-0 1) (zero-until-0 0 L)]
           [(zero-until-0 0) (go-left 1 L)]
           [(zero-until-0 '☠) (go-back-right-once 1 L)]
           [(go-back-right-once '☠) (ones-complement2 '☠ R)]
           [(go-left 0) (go-left 0 L)]
           [(go-left 1) (go-left 1 L)]
           [(go-left '☠) (ones-complement2 '☠ R)]
           ;; Back to sub1
           [(ones-complement2 1)  (ones-complement2 0 R)]
           [(ones-complement2 0)  (ones-complement2 1 R)]
           [(ones-complement2 '☠) (new-go-left '☠ L)]
           [(new-go-left 0) (new-go-left 0 L)]
           [(new-go-left 1) (new-go-left 1 L)]
           [(new-go-left '☠) (remove-zeros '☠ R)]
           [(remove-zeros '☠) (move-right&stop 0 L)]
           [(remove-zeros 0) (remove-zeros '☠ R)]
           [(remove-zeros 1) (move-right&stop 1 L)]
           [(move-right&stop '☠) (#t '☠ R)])))

  (show binary-sub1 '(0 1 0 0 0 1 1 0))

  (define (n->binary n)
    (map string->number (map string (string->list (number->string n 2)))))

  (for ([i (in-range 64)])
    (define i-in-bin (n->binary i))
    (chk (run binary-add1 i-in-bin) (n->binary (add1 i)))
    (unless (negative? (sub1 i))
      (chk (run binary-sub1 i-in-bin) (n->binary (sub1 i)))))
  (for ([_ (in-range 100)])
    (define i (random (expt 2 20)))
    (define i-in-bin (n->binary i))
    (chk (run binary-add1 i-in-bin) (n->binary (add1 i)))
    (unless (negative? (sub1 i))
      (chk (run binary-sub1 i-in-bin) (n->binary (sub1 i)))))

  )
