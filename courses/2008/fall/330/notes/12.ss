; Contains the code for the start of lecture 12
(halt-on-errors true)

#|
<BCFAE> ::= <num> 
| {+ <BCFAE> <BCFAE>} 
| {* <BCFAE> <BCFAE>} 
| <id> 
| {fun {<id>} <BCFAE>} 
| {<BCFAE> <BCFAE>} 
| {if0 <BCFAE> <BCFAE> <BCFAE>} 
| {with {<id> <BCFAE>} <BCFAE>}
| {newbox <BCFAE>}
| {setbox <BCFAE> <BCFAE>}
| {openbox <BCFAE>}
| {seqn <BCFAE> <BCFAE>}
where id is not +, *, fun, if0, with, newbox, setbox, openbox, seqn
|#

(define-type BCFAE
  [num (n number?)]
  [add (lhs BCFAE?)
       (rhs BCFAE?)]
  [mult (lhs BCFAE?)
        (rhs BCFAE?)]
  [id (sym symbol?)]
  [fun (param symbol?)
       (body BCFAE?)]
  [app (fun-expr BCFAE?)
       (arg-expr BCFAE?)]
  [if0 (test BCFAE?)
       (truth BCFAE?)
       (else BCFAE?)]
  [newbox (init BCFAE?)]
  [setbox (box-expr BCFAE?)
          (val-expr BCFAE?)]
  [openbox (box-expr BCFAE?)]
  [seqn (fst BCFAE?)
        (snd BCFAE?)])

(define-type BCFAE-value
  [numV (n number?)]
  [closureV (param symbol?)
            (body BCFAE?)
            (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value BCFAE-value?)
        (env Env?)])

;; valid? : sexp -> boolean
;; determines if valid symbol
(define (valid? se)
  (and (symbol? se)
       (not (member se '(+ * fun if0 with newbox setbox openbox seqn)))))

(test (valid? '+) false)
(test (valid? '*) false)
(test (valid? 'fun) false)
(test (valid? 'if0) false)
(test (valid? 'with) false)
(test (valid? 'newbox) false)
(test (valid? 'setbox) false)
(test (valid? 'openbox) false)
(test (valid? 'seqn) false)
(test (valid? '(x)) false)
(test (valid? 'x) true)

;; parse : sexp -> BCFAE
;; parses into BCFAE
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
    [`(if0 ,test ,truth ,else)
     (if0 (parse test) (parse truth) (parse else))]
    ; Extra syntax
    [`(with (,(and id (? valid?)) ,named-expr) ,body)
     (app (fun id (parse body)) (parse named-expr))]
    [`(newbox ,init-expr)
     (newbox (parse init-expr))]
    [`(setbox ,box-expr ,val-expr)
     (setbox (parse box-expr) (parse val-expr))]
    [`(openbox ,box-expr)
     (openbox (parse box-expr))]
    [`(seqn ,fst ,snd)
     (seqn (parse fst) (parse snd))]
    ; This catches earlier cases, so we catch it here.
    [`(,fun-expr ,arg-expr)
     (app (parse fun-expr) (parse arg-expr))]
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
(test (parse '{with {x 5} 7}) (app (fun 'x (num 7)) (num 5)))
(test/exn (parse '{with {x 5} 7 8}) "syntax")
(test/exn (parse '{with {x 5 9} 7}) "syntax")
(test/exn (parse '{with {with 5} 7}) "syntax")
(test/exn (parse '{with {7 5} 7}) "syntax")
(test/exn (parse '{with {} 7 8}) "syntax")
(test/exn (parse '{with 6}) "syntax")
(test (parse '{newbox 5}) (newbox (num 5)))
(test/exn (parse '{newbox}) "syntax")
(test/exn (parse '{newbox 5 6}) "syntax")
(test (parse '{setbox 5 6}) (setbox (num 5) (num 6)))
(test/exn (parse '{setbox 5 6 7}) "syntax")
(test/exn (parse '{setbox 5}) "syntax")
(test/exn (parse '{setbox}) "syntax")
(test (parse '{openbox 5}) (openbox (num 5)))
(test/exn (parse '{openbox}) "syntax")
(test/exn (parse '{openbox 5 6}) "syntax")
(test (parse '{seqn 5 6}) (seqn (num 5) (num 6)))
(test/exn (parse '{seqn 5 6 7}) "syntax")
(test/exn (parse '{seqn 5}) "syntax")
(test/exn (parse '{seqn}) "syntax")

;; num-lift : (num num -> num) -> BCFAE-value BCFAE-value -> BCFAE-value
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

;; num-zero? : BCFAE-value (only numV) -> boolean
;; lifts zero? to numVs
(define (num-zero? v)
  (if (numV? v)
      (zero? (numV-n v))
      (error 'num-zero? "Given non-number")))

(test (num-zero? (numV 0)) true)
(test (num-zero? (numV 1)) false)
(test/exn (num-zero? (closureV 'x (num 5) (mtSub))) "non-number")

;; lookup : symbol env -> BCFAE-value
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

;; interp : BCFAE env -> BCFAE-value
(define (interp expr env)
  (type-case 
   BCFAE expr
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
   [else
    (error 'interp "Cannot handle ~e~n" expr)]))

;; top-interp : sexp -> BCFAE-value
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
