#lang racket/base
(require racket/list
         racket/match)
(module+ test
  (require chk)
  (define ((t* *) actual-str expected-se)
    (chk (* (open-input-string actual-str))
         expected-se)))

;; Sexp = Symbol
;;      | Number
;;      | ( Sexp ... )

(define (read-until-space ip)
  (match (peek-char ip)
    [(or #\( #\space #\))
     empty]
    [_ (cons (read-char ip) (read-until-space ip))]))
(define (se-read ip)
  (match (peek-char ip)
    [#\(
     (read-char ip)
     (define (read-until-rparen)
       (match (peek-char ip)
         [#\)
          (read-char ip)
          empty]
         [#\space
          (read-char ip)
          (read-until-rparen)]
         [_
          (cons (se-read ip)
                (read-until-rparen))]))
     (read-until-rparen)]
    [#\) (error 'parse "Unbalanced paren")]
    [#\space (read-char) (se-read ip)]
    [some-char
     (define list-of-cs (read-until-space ip))
     (define str (list->string list-of-cs))
     (if (andmap char-numeric? list-of-cs)
       (string->number str)
       (string->symbol str))]))

(module+ test
  (define tse (t* se-read))
  (tse "(()(()()())())"
       '(()(()()())()))
  (tse "(let ((x 17)) (+ x x))"
       '(let ((x 17)) (+ x x))))

;; Cexp = Symbol
;;      | Number
;;      | Cexp + Cexp
;;      | Cexp - Cexp
(define (c-read-atom ip)
  (match (peek-char ip)
    [(? char-alphabetic?)
     (string->symbol (string (read-char ip)))]
    [(? char-numeric?)
     (- (char->integer (read-char ip)) (char->integer #\0))]
    [_
     #f]))

(define (c-read ip)
  (define a (c-read-atom ip))
  (match (peek-char ip)
    [#\+
     (read-char ip)
     (list '+ a (c-read ip))]
    [#\-
     (read-char ip)
     (list '- a (c-read ip))]
    [(? eof-object?)
     a]))

(module+ test
  (define tc (t* c-read))
  (tc "x"
      'x)
  (tc "7"
      7)
  (tc "4+4"
      '(+ 4 4))
  (tc "4-4"
      '(- 4 4))
  #;(tc "4+4-4"
        '(- (+ 4 4) 4))
  #;(tc "x+y-z"
        '(- (+ x y) z)))

(define (parse-id ip)
  (define c (peek-char ip))
  (if (and (char? c) (char-alphabetic? c))
    (string->symbol
     (string (read-char ip)))
    (fail)))

(define (parse-num ip)
  (define c (peek-char ip))
  (if (and (char? c) (char-numeric? c))
    (string->number
     (string (read-char ip)))
    (fail)))

(define (parse-add ip) (fail))
(define (parse-sub ip) (fail))

(define (parse-or ip opts)
  (cond
    [(empty? opts) (fail)]
    [else
     (define opt (first opts))
     (define old-fail fail)
     (define this-pos (file-position ip))
     (or (let/cc this-fail
           (set! fail
                 (λ ()
                   (set! fail old-fail)
                   (file-position ip this-pos)
                   (this-fail #f)))
           (opt ip))
         (parse-or ip (rest opts)))]))

(define (fail) (error 'parse "Couldn't"))

(define (c-read2 ip)
  (define
    ans
    (parse-or
     ip
     (list parse-id
           parse-num
           parse-add
           parse-sub)))
  (unless (eof-object? (peek-char ip))
    (fail))
  ans)

#;(module+ test
    (define tc2 (t* c-read2))
    (tc2 "x"
         'x)
    (tc2 "7"
         7)
    (tc2 "4+4"
         '(+ 4 4))
    (tc2 "4-4"
         '(- 4 4))
    (tc2 "4+4-4"
         '(- (+ 4 4) 4))
    (tc2 "x+y-z"
         '(- (+ x y) z))
    "x+x+x+x+x+x+x+x+x+x+x+x+x+x+1")

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(define-tokens c-val-tokens (NUM VAR))
(define-empty-tokens c-op-tokens (ADD SUB EOF))
(define c-lexer
  (lexer
   [(eof) 'EOF]
   ["+" 'ADD]
   ["-" 'SUB]
   [(:/ "0" "9")
    (token-NUM (string->number lexeme))]
   [(:/ "a" "z")
    (token-VAR (string->symbol lexeme))]))
(define c-parser
  (parser
   (tokens c-val-tokens c-op-tokens)
   (start cexp)
   (error
    (λ (a b c)
      (error 'c-parse "Can't: ~v ~v ~v" a b c)))
   (end EOF)
   (precs (left ADD SUB))
   (grammar
    (cexp [(NUM) $1]
          [(VAR) $1]
          [(cexp ADD cexp) (list '+ $1 $3)]
          [(cexp SUB cexp) (list '- $1 $3)]))))

(define (c-read3 ip)
  (c-parser (λ () (c-lexer ip))))

(module+ test
  (define tc3 (t* c-read3))
  (tc3 "x"
       'x)
  (tc3 "7"
       7)
  (tc3 "4+4"
       '(+ 4 4))
  (tc3 "4-4"
       '(- 4 4))
  (tc3 "4+4-4"
       '(- (+ 4 4) 4))
  (tc3 "x+y-z"
       '(- (+ x y) z))
  (tc3 "4-4+4"
       '(+ (- 4 4) 4)))


