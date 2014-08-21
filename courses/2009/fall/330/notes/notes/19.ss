;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
; Contains the code for the start of lecture 19
(halt-on-errors true)

(define-type CFAE
  [num (n number?)]
  [add (lhs CFAE?)
       (rhs CFAE?)]
  [id (sym symbol?)]
  [fun (param symbol?)
       (body CFAE?)]
  [app (fun-expr CFAE?)
       (arg-expr CFAE?)]
  [if0 (test CFAE?)
       (truth CFAE?)
       (else CFAE?)])

(define-type CFAE-value
  [numV (n number?)]
  [closureV (p procedure?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value CFAE-value?)
        (env Env?)])

#|
<CFAE> ::= <num> 
| {+ <CFAE> <CFAE>} 
| <id> 
| {fun {<id>} <CFAE>} 
| {<CFAE> <CFAE>} 
| {if0 <CFAE> <CFAE> <CFAE>} 
| {with {<id> <CFAE>} <CFAE>}
where id is not +, *, fun, if0, rec, with
|#

;; valid? : sexp -> boolean
;; determines if valid symbol
(define (valid? se)
  (and (symbol? se)
       (not (member se '(+ * fun if0 rec with)))))

(test (valid? '+) false)
(test (valid? '*) false)
(test (valid? 'fun) false)
(test (valid? 'if0) false)
(test (valid? 'rec) false)
(test (valid? 'with) false)
(test (valid? '(x)) false)
(test (valid? 'x) true)

;; parse : sexp -> CFAE
;; parses into CFAE
(define (parse se)
  (match se
    [(? number?)
     (num se)]
    [`(+ ,lhs ,rhs)
     (add (parse lhs) (parse rhs))]
    [(? valid?)
     (id se)]
    [`(fun (,(and param (? valid?))) ,body) 
     (fun param (parse body))]
    [`(,fun-expr ,arg-expr)
     (app (parse fun-expr) (parse arg-expr))]
    [`(if0 ,test ,truth ,else)
     (if0 (parse test) (parse truth) (parse else))]   
    ; Extra syntax
    [`(with (,(and id (? valid?)) ,named-expr) ,body)
     (app (fun id (parse body)) (parse named-expr))]
    [_
     (error 'parse "Invalid syntax: ~e" se)]))

(test (parse 5) (num 5))
(test (parse '{+ 4 6}) (add (num 4) (num 6)))
(test/exn (parse '{+ 4}) "syntax")
(test/exn (parse '{+ 4 5 6}) "syntax")
(test (parse 'fac) (id 'fac))
(test/exn (parse 'with) "syntax")
(test (parse '{fun {x} 5}) (fun 'x (num 5)))
(test/exn (parse '{fun {with} 5}) "syntax")
(test/exn (parse '{fun {} 5}) "syntax")
(test/exn (parse '{fun {x} 5 6}) "syntax")
(test (parse '{5 6}) (app (num 5) (num 6)))
(test/exn (parse '{5}) "syntax")
(test/exn (parse '{5 6 7}) "syntax")
(test (parse '{if0 1 2 3}) (if0 (num 1) (num 2) (num 3)))
(test/exn (parse '{if0 1}) "syntax")
(test/exn (parse '{if0 1 2}) "syntax")
(test/exn (parse '{if0 1 2 3 4}) "syntax")
(test (parse '{with {x 5} 7}) (app (fun 'x (num 7)) (num 5)))
(test/exn (parse '{with {x 5} 7 8}) "syntax")
(test/exn (parse '{with {x 5 9} 7}) "syntax")
(test/exn (parse '{with {with 5} 7}) "syntax")
(test/exn (parse '{with {7 5} 7}) "syntax")
(test/exn (parse '{with {} 7 8}) "syntax")
(test/exn (parse '{with 6}) "syntax")

;; num-lift : (num num -> num) -> CFAE-value CFAE-value -> CFAE-value
;; lifts a scheme operator to numVs
(define (num-lift op)
  (lambda (l r)
    (if (and (numV? l) (numV? r))
        (numV (op (numV-n l) (numV-n r)))
        (error 'num-lift "Cannot operate on non-numbers"))))

(define num+ (num-lift +))
(test (num+ (numV 1) (numV 2)) (numV 3))
(test/exn (num+ (closureV (lambda _ (num 5))) (numV 2)) "non-number")
(test/exn (num+ (numV 2) (closureV (lambda _ (num 5)))) "non-number")

;; num-zero? : CFAE-value (only numV) -> boolean
;; lifts zero? to numVs
(define (num-zero? v)
  (if (numV? v)
      (zero? (numV-n v))
      (error 'num-zero? "Given non-number")))

(test (num-zero? (numV 0)) true)
(test (num-zero? (numV 1)) false)
(test/exn (num-zero? (closureV (lambda _ (num 5)))) "non-number")

;; lookup : symbol env -> CFAE-value
;; looks up a value in the environment
(define (lookup name env)
  (type-case 
   Env env
   [mtSub () (error 'lookup "no binding for identifier: ~e" name)]
   [aSub (bound-name bound-value env)
         (if (symbol=? bound-name name)
             bound-value
             (lookup name env))]))

(test/exn (lookup 'x (mtSub)) "binding")
(test (lookup 'x (aSub 'x (numV 5) (mtSub))) (numV 5))
(test/exn (lookup 'x (aSub 'y (numV 5) (mtSub))) "binding")

;; interp : CFAE env -> CFAE-value
(define (interp expr env)
  (type-case 
   CFAE expr
   [num (n) (numV n)]
   [add (l r) (num+ (interp l env) 
                    (interp r env))]
   [if0 (test truth else)
        (if (num-zero? (interp test env))
            (interp truth env)
            (interp else env))]
   [id (v) (lookup v env)]
   [fun (bound-id bound-body)
        (closureV (lambda (arg-val) (interp bound-body
                                            (aSub bound-id
                                                  arg-val
                                                  env))))]
   [app (fun-expr arg-expr)
        (type-case 
         CFAE-value (interp fun-expr env)
         [closureV (c) (c (interp arg-expr env))]
         [else (error 'interp "Not an applicable value")])]))

;; id : any -> any
(define (id x) x)

;; top-interp : sexp -> CFAE-value
(define (top-interp se)
  (interp (parse se) (mtSub)))

;; interp tests
(test (top-interp '5) (numV 5))
(test/pred (top-interp '{fun {x} x}) closureV?)

(test (top-interp '{+ 4 5}) (numV 9))
(test/exn (top-interp '{+ 4 {fun {x} x}}) "non-number")
(test/exn (top-interp '{+ {fun {x} x} 4}) "non-number")

(test (top-interp '(if0 0 1 2)) (numV 1))
(test (top-interp '(if0 1 1 2)) (numV 2))
(test (top-interp '(if0 (+ 1 -1) 1 2)) (numV 1))
(test (top-interp '(if0 (+ 1 1) 1 2)) (numV 2))

(test (interp (parse 'x) 
              (aSub 'x (numV 1) (mtSub)))
      (numV 1))
(test/exn (top-interp 'x) "binding")

(test (top-interp '{{fun {x} x} 5}) (numV 5))
(test (top-interp '{with {x 5} x}) (numV 5))
(test/pred (top-interp '{with {x 5} {fun {y} {+ x y}}})
           closureV?)

(test (top-interp '{with {x 5} {with {x 2} x}}) (numV 2))
(test (top-interp '{with {x 5} {with {y x} y}}) (numV 5))
(test (top-interp '{with {x 5} {with {x {+ x 1}} x}}) (numV 6))

(test/exn (top-interp '{with {fac {fun {n}
                                       {if0 n
                                            1
                                            {+ n {fac {+ n -1}}}}}}
                             {fac 3}})
          "binding")

;; bindcc tests
#|
(test (top-interp '{bindcc k 3}) (numV 3))
(test (top-interp '{bindcc k (k 3)}) (numV 3))
(test (top-interp '{bindcc k {+ 1 {k 3}}} (numV 3)))
(test (top-interp '{+ 1 {bindcc k {+ 1 {k 3}}}}) (numV 4))

(test (top-interp '{{bindcc k
                            {k {fun {dummy}
                                    3}}}
                    1830})
      (numV 3))
(test (top-interp '{bindcc k
                           {k
                            {k
                             {k 3}}}})
      (numV 3))
(test (top-interp '{{{bindcc k k}
                     {fun {x} x}}
                    3})
      (numV 3))
(test (top-interp '{{{{bindcc k k}
                      {fun {x} x}}
                     {fun {x} x}}
                    3})    
|#