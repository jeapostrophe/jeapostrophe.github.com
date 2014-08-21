; Contains the code for the start of lecture 11
(halt-on-errors true)

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?)
       (rhs RCFAE?)]
  [mult (lhs RCFAE?)
        (rhs RCFAE?)]
  [id (sym symbol?)]
  [fun (param symbol?)
       (body RCFAE?)]
  [app (fun-expr RCFAE?)
       (arg-expr RCFAE?)]
  [if0 (test RCFAE?)
       (truth RCFAE?)
       (else RCFAE?)]
  [rec (bound-id symbol?)
    (named-expr RCFAE?)
    (body RCFAE?)])

(define-type RCFAE-value
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (env Env?)])

(define (boxed-RCFAE-value? v)
  (and (box? v)
       (RCFAE-value? (unbox v))))

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value RCFAE-value?)
        (env Env?)]
  [aRecSub (name symbol?)
           (value boxed-RCFAE-value?)
           (env Env?)])

#|
<RCFAE> ::= <num> 
| {+ <RCFAE> <RCFAE>} 
| {* <RCFAE> <RCFAE>} 
| <id> 
| {fun {<id>} <RCFAE>} 
| {<RCFAE> <RCFAE>} 
| {if0 <RCFAE> <RCFAE> <RCFAE>} 
| {rec {<id> <RCFAE>} <RCFAE>} 
| {with {<id> <RCFAE>} <RCFAE>}
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

;; parse : sexp -> RCFAE
;; parses into RCFAE
(define (parse se)
  (match se
    [(? number?)
     (num se)]
    [`(+ ,lhs ,rhs)
     (add (parse lhs) (parse rhs))]
    [`(* ,lhs ,rhs)
     (mult (parse lhs) (parse rhs))]
    [(? valid?)
     (id se)]
    [`(fun (,(and param (? valid?))) ,body) 
     (fun param (parse body))]
    [`(,fun-expr ,arg-expr)
     (app (parse fun-expr) (parse arg-expr))]
    [`(if0 ,test ,truth ,else)
     (if0 (parse test) (parse truth) (parse else))]
    [`(rec (,(and id (? valid?)) ,named-expr) ,body)
     (rec id (parse named-expr) (parse body))]
    ; Extra syntax
    [`(with (,(and id (? valid?)) ,named-expr) ,body)
     (app (fun id (parse body)) (parse named-expr))]
    [_
     (error 'parse "Invalid syntax: ~e" se)]))

(test (parse 5) (num 5))
(test (parse '{+ 4 6}) (add (num 4) (num 6)))
(test/exn (parse '{+ 4}) "syntax")
(test/exn (parse '{+ 4 5 6}) "syntax")
(test (parse '{* 4 6}) (mult (num 4) (num 6)))
(test/exn (parse '{* 4}) "syntax")
(test/exn (parse '{* 4 5 6}) "syntax")
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
(test (parse '{rec {x 5} 7}) (rec 'x (num 5) (num 7)))
(test/exn (parse '{rec {x 5} 7 8}) "syntax")
(test/exn (parse '{rec {x 5 9} 7}) "syntax")
(test/exn (parse '{rec {with 5} 7}) "syntax")
(test/exn (parse '{rec {7 5} 7}) "syntax")
(test/exn (parse '{rec {} 7 8}) "syntax")
(test/exn (parse '{rec 6}) "syntax")
(test (parse '{with {x 5} 7}) (app (fun 'x (num 7)) (num 5)))
(test/exn (parse '{with {x 5} 7 8}) "syntax")
(test/exn (parse '{with {x 5 9} 7}) "syntax")
(test/exn (parse '{with {with 5} 7}) "syntax")
(test/exn (parse '{with {7 5} 7}) "syntax")
(test/exn (parse '{with {} 7 8}) "syntax")
(test/exn (parse '{with 6}) "syntax")

;; num-lift : (num num -> num) -> RCFAE-value RCFAE-value -> RCFAE-value
;; lifts a scheme operator to numVs
(define (num-lift op)
  (lambda (l r)
    (if (and (numV? l) (numV? r))
        (numV (op (numV-n l) (numV-n r)))
        (error 'num-lift "Cannot operate on non-numbers"))))

(define num+ (num-lift +))
(test (num+ (numV 1) (numV 2)) (numV 3))
(test/exn (num+ (closureV 'x (num 5) (mtSub)) (numV 2)) "non-number")
(test/exn (num+ (numV 2) (closureV 'x (num 5) (mtSub))) "non-number")

(define num* (num-lift *))
(test (num* (numV 1) (numV 2)) (numV 2))
(test/exn (num* (closureV 'x (num 5) (mtSub)) (numV 2)) "non-number")
(test/exn (num* (numV 2) (closureV 'x (num 5) (mtSub))) "non-number")

;; num-zero? : RCFAE-value (only numV) -> boolean
;; lifts zero? to numVs
(define (num-zero? v)
  (if (numV? v)
      (zero? (numV-n v))
      (error 'num-zero? "Given non-number")))

(test (num-zero? (numV 0)) true)
(test (num-zero? (numV 1)) false)
(test/exn (num-zero? (closureV 'x (num 5) (mtSub))) "non-number")

;; lookup : symbol env -> RCFAE-value
;; looks up a value in the environment
(define (lookup name env)
  (type-case 
   Env env
   [mtSub () (error 'lookup "no binding for identifier: ~e" name)]
   [aSub (bound-name bound-value env)
         (if (symbol=? bound-name name)
             bound-value
             (lookup name env))]
   [aRecSub (bound-name boxed-bound-value env)
            (if (symbol=? bound-name name)
                (unbox boxed-bound-value)
                (lookup name env))]))

(test/exn (lookup 'x (mtSub)) "binding")
(test (lookup 'x (aSub 'x (numV 5) (mtSub))) (numV 5))
(test/exn (lookup 'x (aSub 'y (numV 5) (mtSub))) "binding")

;; cylically-bind-and-interp : symbol RCFAE env -> env
(define (cylically-bind-and-interp bound-id named-expr env)
  (local [(define value-holder (box (numV 1830)))
          (define new-env (aRecSub bound-id value-holder env))
          (define named-expr-val (interp named-expr new-env))]
    (begin (set-box! value-holder named-expr-val)
           new-env)))

;; interp : RCFAE env -> RCFAE-value
(define (interp expr env)
  (type-case 
   RCFAE expr
   [num (n) (numV n)]
   [add (l r) (num+ (interp l env) 
                    (interp r env))]
   [mult (l r) (num* (interp l env) 
                     (interp r env))]
   [if0 (test truth else)
        (if (num-zero? (interp test env))
            (interp truth env)
            (interp else env))]
   [id (v) (lookup v env)]
   [fun (bound-id bound-body)
        (closureV bound-id bound-body env)]
   [app (fun-expr arg-expr)
        (local [(define clos (interp fun-expr env))]
          (interp (closureV-body clos)
                  (aSub (closureV-param clos)
                        (interp arg-expr env)
                        (closureV-env clos))))]
   [rec (bound-id named-expr bound-body)
     (interp bound-body
             (cylically-bind-and-interp bound-id
                                        named-expr
                                        env))]))

;; top-interp : sexp -> RCFAE-value
(define (top-interp se)
  (interp (parse se) (mtSub)))

;; interp tests
(test (top-interp '5) (numV 5))
(test (top-interp '{fun {x} x}) (closureV 'x (id 'x) (mtSub)))

(test (top-interp '{+ 4 5}) (numV 9))
(test/exn (top-interp '{+ 4 {fun {x} x}}) "non-number")
(test/exn (top-interp '{+ {fun {x} x} 4}) "non-number")

(test (top-interp '{* 4 5}) (numV 20))
(test/exn (top-interp '{* 4 {fun {x} x}}) "non-number")
(test/exn (top-interp '{* {fun {x} x} 4}) "non-number")

(test (top-interp '(if0 0 1 2)) (numV 1))
(test (top-interp '(if0 1 1 2)) (numV 2))
(test (top-interp '(if0 (+ 1 -1) 1 2)) (numV 1))
(test (top-interp '(if0 (+ 1 1) 1 2)) (numV 2))

(test (interp (parse 'x) (aSub 'x (numV 1) (mtSub))) (numV 1))
(test/exn (top-interp 'x) "binding")

(test (top-interp '{{fun {x} x} 5}) (numV 5))
(test (top-interp '{with {x 5} x}) (numV 5))
(test (top-interp '{with {x 5} {fun {y} {+ x y}}}) 
      (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 5) (mtSub))))

(test (top-interp '{with {x 5} {with {x 2} x}}) (numV 2))
(test (top-interp '{with {x 5} {with {y x} y}}) (numV 5))
(test (top-interp '{with {x 5} {with {x {+ x 1}} x}}) (numV 6))

(test/exn (top-interp '{with {fac {fun {n}
                                       {if0 n
                                            1
                                            {* n {fac {+ n -1}}}}}}
                             {fac 3}})
          "binding")
(test (top-interp '{rec {fac {fun {n}
                                  {if0 n
                                       1
                                       {* n {fac {+ n -1}}}}}}
                     {fac 3}})
      (numV 6))