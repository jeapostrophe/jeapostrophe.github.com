#lang plai
(halt-on-errors true)

(define-type Expr
  [num (n number?)]
  [id (v symbol?)]
  [bool (b boolean?)]
  [bin-num-op (op procedure?) (lhs Expr?) (rhs Expr?)]
  [iszero (e Expr?)]
  [bif (test Expr?) (then Expr?) (else Expr?)]
  [with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
  [rec-with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
  [fun (arg-id symbol?) (body Expr?)]
  [app (fun-expr Expr?) (arg-expr Expr?)]
  [tempty]
  [tcons (first Expr?) (rest Expr?)]
  [tfirst (e Expr?)]
  [trest (e Expr?)]
  [istempty (e Expr?)])

(define-type Type
  [t-num]
  [t-bool]
  [t-list (elem Type?)]
  [t-fun (arg Type?) (result Type?)]
  [t-var (v symbol?)])

#| <expr> ::= <num>
          | true
          | false
          | {+ <expr> <expr>}
          | {- <expr> <expr>}
          | {* <expr> <expr>}
          | {iszero <expr>}
          | {bif <expr> <expr> <expr>}

          | <id>
          | {with {<id> <expr>} <expr>}
          | {rec {<id> <expr>} <expr>}
          | {fun {<id>} <expr>}
          | {<expr> <expr>}

          | tempty
          | {tcons <expr> <expr>}
          | {tempty? <expr>}
          | {tfirst <expr>}
          | {trest <expr>}
|#

; parse :: S-exp -> Expr
(define parse
  (match-lambda
    [(? number? n) (num n)]
    ['true (bool #t)]
    ['false (bool #f)]
    [`(+ ,lhs ,rhs)
     (bin-num-op + (parse lhs) (parse rhs))]
    [`(- ,lhs ,rhs)
     (bin-num-op - (parse lhs) (parse rhs))]
    [`(* ,lhs ,rhs)
     (bin-num-op * (parse lhs) (parse rhs))]
    [`(iszero ,expr)
     (iszero (parse expr))]
    [`(bif ,c ,t ,f)
     (bif (parse c) (parse t) (parse f))]
    [`(with (,name ,named-expr) ,body)
     (with name (parse named-expr) (parse body))]
    [`(rec (,name ,named-expr) ,body)
     (rec-with name (parse named-expr) (parse body))]
    [`(fun (,arg) ,body)
     (fun arg (parse body))]
    [`tempty
     (tempty)]
    [`(tcons ,fst ,rst)
     (tcons (parse fst) (parse rst))]
    [`(tempty? ,expr)
     (istempty (parse expr))]
    [`(tfirst ,expr)
     (tfirst (parse expr))]
    [`(trest ,expr)
     (trest (parse expr))]
    [`(,fun ,arg)
     (app (parse fun) (parse arg))]
    [(? symbol? s)
     (id s)]
    [else
     (error 'parse "Cannot parse")]))

(test (parse 5) (num 5))

(test (parse 'true) (bool #t))
(test (parse 'false) (bool #f))

(test (parse '(+ 1 2)) (bin-num-op + (num 1) (num 2)))
(test (parse '(- 1 2)) (bin-num-op - (num 1) (num 2)))
(test (parse '(* 1 2)) (bin-num-op * (num 1) (num 2)))
(test (parse '(+ true 2)) (bin-num-op + (bool #t) (num 2)))
(test (parse '(+ 1 true)) (bin-num-op + (num 1) (bool #t)))

(test (parse '(iszero 0)) (iszero (num 0)))
(test (parse '(iszero 1)) (iszero (num 1)))
(test (parse '(iszero true)) (iszero (bool #t)))

(test (parse '(bif true 1 2)) (bif (bool #t) (num 1) (num 2)))
(test (parse '(bif 1 1 2)) (bif (num 1) (num 1) (num 2)))
(test (parse '(bif true 1 false)) (bif (bool #t) (num 1) (bool #f)))

(test (parse 'x) (id 'x))

(test (parse '(with (x 1) 1)) (with 'x (num 1) (num 1)))
(test (parse '(with (x true) (iszero x))) (with 'x (bool #t) (iszero (id 'x))))
(test (parse '(with (x 1) x)) (with 'x (num 1) (id 'x)))

(test (parse '(rec (x 1) x)) (rec-with 'x (num 1) (id 'x)))

(test (parse '(fun (x) x)) (fun 'x (id 'x)))
(test (parse '(fun (x) 1)) (fun 'x (num 1)))
(test (parse '(fun (x) (+ x 1))) (fun 'x (bin-num-op + (id 'x) (num 1))))

(test (parse '((fun (x) x) 1)) (app (fun 'x (id 'x)) (num 1)))
(test (parse '((fun (x) (iszero x)) true)) (app (fun 'x (iszero (id 'x))) (bool #t)))

(test (parse '(with (x (fun (y) (x y))) 1))
      (with 'x (fun 'y (app (id 'x) (id 'y))) (num 1)))
(test (parse '(rec (fac (fun (n) (bif (iszero n) 1 (* n (fac (- n 1)))))) (fac 15)))
      (rec-with 'fac (fun 'n (bif (iszero (id 'n)) (num 1) (bin-num-op * (id 'n) (app (id 'fac) (bin-num-op - (id 'n) (num 1))))))
                (app (id 'fac) (num 15))))

(test (parse 'tempty) (tempty))

(test (parse '(tcons 1 tempty)) (tcons (num 1) (tempty)))
(test (parse '(tcons 1 1)) (tcons (num 1) (num 1)))
(test (parse '(tcons 1 (tcons true tempty))) (tcons (num 1) (tcons (bool #t) (tempty))))

(test (parse '(tempty? tempty)) (istempty (tempty)))
(test (parse '(tempty? (tcons 1 tempty))) (istempty (tcons (num 1) (tempty))))
(test (parse '(tempty? 1)) (istempty (num 1)))

(test (parse '(tfirst tempty)) (tfirst (tempty)))
(test (parse '(tfirst (tcons 1 tempty))) (tfirst (tcons (num 1) (tempty))))
(test (parse '(tfirst 1)) (tfirst (num 1)))

(test (parse '(trest tempty)) (trest (tempty)))
(test (parse '(trest (tcons 1 tempty))) (trest (tcons (num 1) (tempty))))
(test (parse '(trest 1)) (trest (num 1)))

(define-type Constraint
  [EqC (lhs Type?) (rhs Type?)])

; a renamer is a hash from symbol to symbol

; generate-constraints :: symbol expr -> (listof Constraint?)
(define (generate-constraints label expr)
  ; gc :: renamer symbol expr -> (listof Constraint?)
  (define (gc renamer label expr)
    (type-case Expr expr
      [num (n) empty]
      [id (v) empty]
      [bool (b) empty]
      [bin-num-op (op lhs rhs)
                  empty]
      [iszero (e) empty]
      [bif (test then else) empty]
      [with (bound-id bound-body body) empty]
      [rec-with (bound-id bound-body body) empty]
      [fun (arg-id body) empty]
      [app (fun-e arg-e) empty]
      [tempty () empty]
      [tcons (first-expr rest-expr) empty]
      [tfirst (e) empty]
      [trest (e) empty]
      [istempty (e) empty]))
  (gc (make-immutable-hash empty)
      label
      expr))

; replace-type-in-type : symbol Type Type -> Type
(define (replace-type-in-type sym sym-t t)
  (type-case Type t
    [t-num () #f]
    [t-bool () #f]
    [t-list (elem) #f]
    [t-fun (dom rng) #f]
    [t-var (v) #f]))

; replace-type-in-constraint : symbol Type -> Constraint -> Constraint
(define ((replace-type-in-constraint sym sym-t) c)
  (type-case Constraint c
    [EqC (lhs rhs)
         #f]))

; replace-type-in-constraint-list : symbol Type (listof Constraint) -> (listof Constraint)
(define (replace-type-in-constraint-list sym sym-t loc)
  #f)

; a substitution is a hash from symbol to Type

; hash-update : (hash a b) (b -> c) -> (hash a c)
(define (hash-update h f)
  (make-immutable-hash
   (hash-map h (lambda (k v) (cons k (f v))))))

; unify :: (listof Constraint?) -> substitution
(define (unify loc)
  ; step :: (listof Constraint?) substitution? -> substitution?
  (define (step stack subst)
    ; XXX
    subst)
  (step loc (make-immutable-hash empty)))

; infer-type :: Expr -> Type
(define (infer-type expr)
  (define label (gensym 'top))
  (define loc (generate-constraints label expr))
  (define subst (unify loc))
  (hash-ref subst label))

; type=? Type -> Type -> Bool
; signals an error if arguments are not variants of Type
(define ((type=? t1) t2)
  (local ([define ht1 (make-hash)] ; maps vars in t1 to vars in t2
          [define ht2 (make-hash)] ; vice versa
          [define (teq? t1 t2)
            (cond
              [(and (t-num? t1) (t-num? t2)) true]
              [(and (t-bool? t1) (t-bool? t2)) true]
              [(and (t-list? t1) (t-list? t2))
               (teq? (t-list-elem t1) (t-list-elem t2))]
              [(and (t-fun? t1) (t-fun? t2))
               (and (teq? (t-fun-arg t1) (t-fun-arg t2))
                    (teq? (t-fun-result t1) (t-fun-result t2)))]
              [(and (t-var? t1) (t-var? t2))
               (local ([define v1 ; the symbol that ht1 says that t1 maps to
                         (hash-ref 
                          ht1 (t-var-v t1) 
                          (lambda ()
                            ; if t1 doesn't map to anything, it's the first time
                            ; we're seeing it, so map it to t2
                            (hash-set! ht1 (t-var-v t1) (t-var-v t2))
                            (t-var-v t2)))]
                       [define v2
                         (hash-ref
                          ht2 (t-var-v t2)
                          (lambda ()
                            (hash-set! ht2 (t-var-v t2) (t-var-v t1))
                            (t-var-v t1)))])
                 ; we have to check both mappings, so that distinct variables
                 ; are kept distinct. i.e. a -> b should not be isomorphic to
                 ; c -> c under the one-way mapping a => c, b => c.
                 (and (symbol=? (t-var-v t2) v1) (symbol=? (t-var-v t1) v2)))]
              [(and (Type? t1) (Type? t2)) false]
              [else (error 'type=? "either ~a or ~a is not a Type" t1 t2)])])
    (or (teq? t1 t2)
        ; Unfortunately, test/pred simply prints false; this helps us see
        ; what t2 was.
        (error 'type=? "~s and ~a are not equal (modulo renaming)" t1 t2))))

(define-syntax testt
  (syntax-rules ()
    [(testt t1 t2)
     (test/pred t1 (type=? t2))]))

; Infer-type tests
(testt (infer-type (parse 5))
       (t-num))

(testt (infer-type (parse 'true))
       (t-bool))
(testt (infer-type (parse 'false))
       (t-bool))

(testt (infer-type (parse '(+ 1 2))) (t-num))
(testt (infer-type (parse '(- 1 2))) (t-num))
(testt (infer-type (parse '(* 1 2))) (t-num))
(test/exn (infer-type (parse '(+ true 2))) "unify")
(test/exn (infer-type (parse '(+ 1 true))) "unify")

(testt (infer-type (parse '(iszero 0))) (t-bool))
(testt (infer-type (parse '(iszero 1))) (t-bool))
(test/exn (infer-type (parse '(iszero true))) "unify")

(testt (infer-type (parse '(bif true 1 2))) (t-num))
(test/exn (infer-type (parse '(bif 1 1 2))) "unify")
(test/exn (infer-type (parse '(bif true 1 false))) "unify")

(test/exn (infer-type (parse 'x)) "unbound")

(testt (infer-type (parse '(with (x 1) 1))) (t-num))
(testt (infer-type (parse '(with (x true) (iszero x)))) "unify")
(testt (infer-type (parse '(with (x 1) x))) (t-num))

(testt (infer-type (parse '(rec (x 1) x))) (t-num))

(testt (infer-type (parse '(fun (x) x))) (t-fun (t-var 'a) (t-var 'a)))
(testt (infer-type (parse '(fun (x) 1))) (t-fun (t-var 'a) (t-num)))
(testt (infer-type (parse '(fun (x) (+ x 1)))) (t-fun (t-num) (t-num)))

(testt (infer-type (parse '((fun (x) x) 1))) (t-num))
(test/exn (infer-type (parse '((fun (x) (iszero x)) true))) "unify")

(test/exn (parse '(with (x (fun (y) (x y))) 1))
          "unbound")
(testt (infer-type (parse '(rec (fac (fun (n) (bif (iszero n) 1 (* n (fac (- n 1)))))) (fac 15))))
       (t-num))

(testt (infer-type (parse 'tempty)) (t-list (t-var 'a)))

(testt (infer-type (parse '(tcons 1 tempty))) (t-list (t-num)))
(test/exn (infer-type (parse '(tcons 1 1))) "unify")
(test/exn (infer-type (parse '(tcons 1 (tcons true tempty)))) "unify")

(testt (infer-type (parse '(tempty? tempty))) (t-bool))
(testt (infer-type (parse '(tempty? (tcons 1 tempty)))) (t-bool))
(test/exn (infer-type (parse '(tempty? 1))) "unify")

(testt (infer-type (parse '(tfirst tempty))) (t-var 'a))
(testt (infer-type (parse '(tfirst (tcons 1 tempty)))) (t-num))
(test/exn (infer-type (parse '(tfirst 1))) "unify")

(testt (infer-type (parse '(trest tempty))) (t-list (t-var 'a)))
(testt (infer-type (parse '(trest (tcons 1 tempty)))) (t-list (t-num)))
(test/exn (infer-type (parse '(trest 1))) "unify")

; Extra Credit
(testt (infer-type (parse '(fun (a) (tfirst tempty))))
       (t-fun (t-var 'a) (t-var 'b)))