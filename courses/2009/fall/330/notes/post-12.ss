;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
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

;;;; Environments
(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (loc number?)
        (env Env?)])

;; env-lookup : symbol env -> number
;; looks up a value in the environment
(define (env-lookup name env)
  (type-case 
   Env env
   [mtSub () (error 'lookup "no binding for identifier: ~e" name)]
   [aSub (bound-name bound-loc env)
         (if (symbol=? bound-name name)
             bound-loc
             (env-lookup name env))]))

(test/exn (env-lookup 'x (mtSub)) "binding")
(test (env-lookup 'x (aSub 'x 5 (mtSub))) 5)
(test/exn (env-lookup 'x (aSub 'y 5 (mtSub))) "binding")

;;;; Stores
(define-type Store
  [mtStore]
  [aStore (loc number?)
          (value BCFAE-value?)
          (sto Store?)])

(define-type ValueXStore
  [vxs (val BCFAE-value?)
       (sto Store?)])

; next-store-location : store -> number
(define (next-store-location sto)
  (type-case Store sto
             [mtStore () 0]
             [aStore (loc val sto)
                     (max (add1 loc) (next-store-location sto))]))

;; store-lookup : number store -> value
;; looks up a value in the store
(define (store-lookup loc sto)
  (type-case 
   Store sto
   [mtStore () (error 'store-lookup "no binding for loc: ~e" loc)]
   [aStore (mem-loc mem-val sto)
         (if (= mem-loc loc)
             mem-val
             (store-lookup loc sto))]))

(test/exn (store-lookup 5 (mtStore)) "binding")
(test (store-lookup 5 (aStore 5 (numV 6) (mtStore))) (numV 6))
(test/exn (store-lookup 5 (aStore 7 (numV 6) (mtStore))) "binding")

;;;; Parsing

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
    ; This matches earlier cases, so we catch it here.
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

;;;; Interpretation

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

;; interp : BCFAE env store -> ValueXStore
(define (interp expr env sto)
  (type-case 
   BCFAE expr
   [num (n) (vxs (numV n) sto)]
   [add (l r) 
        (type-case ValueXStore (interp l env sto)
                   [vxs (lv lsto)
                        (type-case ValueXStore (interp r env lsto)
                                   [vxs (rv rsto)
                                        (vxs (num+ lv rv) rsto)])])]
   [mult (l r) 
         (type-case ValueXStore (interp l env sto)
                   [vxs (lv lsto)
                        (type-case ValueXStore (interp r env lsto)
                                   [vxs (rv rsto)
                                        (vxs (num* lv rv) rsto)])])]
   [if0 (test truth else)
        (type-case ValueXStore (interp test env sto)
                   [vxs (tv tsto)
                        (if (num-zero? tv)
                            (interp truth env tsto)
                            (interp else env tsto))])]
   [id (v) (vxs (store-lookup (env-lookup v env)
                         sto)
                sto)]
   [fun (bound-id bound-body)
        (vxs (closureV bound-id bound-body env)
             sto)]
   [app (fun-expr arg-expr)
        (type-case ValueXStore (interp fun-expr env sto)
                   [vxs (clos fsto)
                        (type-case ValueXStore (interp arg-expr env fsto)
                                   [vxs (arg-val asto)
                                        (local [(define arg-loc (next-store-location asto))]
                                          (interp (closureV-body clos)
                                                  (aSub (closureV-param clos)
                                                        arg-loc
                                                        (closureV-env clos))
                                                  (aStore arg-loc
                                                          arg-val
                                                          asto)))])])]
   [seqn (fst-expr snd-expr)
         (type-case ValueXStore (interp fst-expr env sto)
                    [vxs (fv fsto)
                         (interp snd-expr env fsto)])]
   [else
    (error 'interp "Cannot handle ~e~n" expr)]))

;; top-interp : sexp -> BCFAE-value
(define (top-interp se)
  (vxs-val (interp (parse se) (mtSub) (mtStore))))

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

(test (vxs-val (interp (parse 'x) (aSub 'x 5 (mtSub)) (aStore 5 (numV 1) (mtStore)))) (numV 1))
(test/exn (top-interp 'x) "binding")

(test (top-interp '{{fun {x} x} 5}) (numV 5))
(test (top-interp '{with {x 5} x}) (numV 5))
(test (top-interp '{with {x 5} {fun {y} {+ x y}}}) 
      (closureV 'y (add (id 'x) (id 'y)) (aSub 'x 0 (mtSub))))

(test (top-interp '{with {x 5} {with {x 2} x}}) (numV 2))
(test (top-interp '{with {x 5} {with {y x} y}}) (numV 5))
(test (top-interp '{with {x 5} {with {x {+ x 1}} x}}) (numV 6))

(test/exn (top-interp '{with {fac {fun {n}
                                       {if0 n
                                            1
                                            {* n {fac {+ n -1}}}}}}
                             {fac 3}})
          "binding")

; Box tests
(test (top-interp '{seqn 1 2})
      (numV 2))
(test (top-interp '{openbox {newbox 1}})
      (numV 1))
(test (top-interp '{setbox {newbox 1} 2})
      (numV 2))
(test (top-interp '{with {b {newbox 0}}
                         {seqn {setbox b {+ 1 {openbox b}}}
                               {openbox b}}})
      (numV 1))
(test/exn (top-interp '{with {a 5}
                             {seqn {with {b 3}
                                         b}
                                   b}})
          "binding")
(test (top-interp '{with {a {newbox 1}}
                         {with {f {fun {y} {+ y {openbox a}}}}
                               {seqn {setbox a 2}
                                     {f 10}}}})
      (numV 12))

(test (top-interp '{with {b {newbox 0}}
                         {with {increment {fun {dum} {with {old {openbox b}}
                                                           {seqn {setbox b {+ 1 old}}
                                                                 old}}}}
                               {seqn {increment 0}
                                     {+ {increment 0}
                                        {seqn {increment 0}
                                              {increment 0}}}}}})
      (numV 4))