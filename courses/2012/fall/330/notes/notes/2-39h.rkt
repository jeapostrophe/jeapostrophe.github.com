#lang plai
(print-only-errors #t)

(define id-se
  `(fun (x) x))
(define map-se
  `(fun (f)
        (fun (l)
             (bif (tempty? l)
                  tempty
                  (tcons (f (tfirst l))
                         ((map f) (trest l)))))))
(define sum-se
  `(fun (l)
        (bif (tempty? l)
             0
             (+ (tfirst l)
                (sum (trest l))))))
(define complex-se
  `(rec (map ,map-se)
        (rec (sum ,sum-se)
             (bif
              (iszero
               ((,id-se sum)
                ((map (fun (l) (tfirst l)))
                 (with (num-tempty tempty)
                       (tcons (tcons (,id-se 1) num-tempty)
                              (tcons (tcons 2 num-tempty)
                                     (tcons (tcons 3 num-tempty)
                                            tempty)))))))
              false
              true))))

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

(define-type Constraint
  [eqc (lhs Type?) (rhs Type?)])

(define (parse se)
  (match se
    ;; expr =           num
    [(? number? se)
     (num se)]
    ;;          |       true
    ['true
     (bool #t)]
    ;;          |       false
    ['false
     (bool #f)]
    ;;          |       (+ expr expr)
    [(list '+ lhs-se rhs-se)
     (bin-num-op + (parse lhs-se) (parse rhs-se))]
    ;;          |       (- expr expr)
    [(list '- lhs-se rhs-se)
     (bin-num-op - (parse lhs-se) (parse rhs-se))]
    ;;          |       (* expr expr)
    [(list '* lhs-se rhs-se)
     (bin-num-op * (parse lhs-se) (parse rhs-se))]
    ;;          |       (iszero expr)
    [(list 'iszero se)
     (iszero (parse se))]
    ;;          |       (bif expr expr expr)
    [(list 'bif cond-se true-se false-se)
     (bif (parse cond-se) (parse true-se) (parse false-se))]
    ;;          |       tempty
    ['tempty
     (tempty)]
    ;;          |       id
    [(? symbol? se)
     (id se)]
    ;;          |       (with (id expr) expr)
    [(list 'with (list bound-id bound-se) body-se)
     (with bound-id (parse bound-se) (parse body-se))]
    ;;          |       (rec (id expr) expr)
    [(list 'rec (list bound-id bound-se) body-se)
     (rec-with bound-id (parse bound-se) (parse body-se))]
    ;;          |       (fun (id) expr)
    [(list 'fun (list param-id) body-se)
     (fun param-id (parse body-se))]
    ;;          |       (tcons expr expr)
    [(list 'tcons first-se rest-se)
     (tcons (parse first-se) (parse rest-se))]
    ;;          |       (tempty? expr)
    [(list 'tempty? se)
     (istempty (parse se))]
    ;;          |       (tfirst expr)
    [(list 'tfirst se)
     (tfirst (parse se))]
    ;;          |       (trest expr)
    [(list 'trest se)
     (trest (parse se))]
    ;;          |       (expr expr)
    [(list fun-se arg-se)
     (app (parse fun-se) (parse arg-se))]
    [_
     (error 'parse "~e" se)]))

(parse complex-se)

(define (alpha-vary e)
  (alpha-vary/env (hasheq) e))

(define (alpha-vary/env env e)
  (type-case
   Expr e
   [num
    (n)
    (num n)]
   [id
    (v)
    (id (hash-ref env v (λ () (error 'nice))))]
   [bool
    (b)
    (bool b)]
   [bin-num-op
    (op lhs rhs)
    (bin-num-op op
                (alpha-vary/env env lhs)
                (alpha-vary/env env rhs))]
   [iszero
    (e)
    (iszero
     (alpha-vary/env env e))]
   [bif
    (test then else)
    (bif
     (alpha-vary/env env test)
     (alpha-vary/env env then)
     (alpha-vary/env env else))]
   [with
    (bound-id bound-body body)
    (begin
      (define new-bound-id (gensym bound-id))
      (with new-bound-id
            (alpha-vary/env env bound-body)
            (alpha-vary/env (hash-set env bound-id new-bound-id) body)))]
   [rec-with
    (bound-id bound-body body)
    (begin
      (define new-bound-id (gensym bound-id))
      (with new-bound-id
            (alpha-vary/env (hash-set env bound-id new-bound-id) bound-body)
            (alpha-vary/env (hash-set env bound-id new-bound-id) body)))]
   [fun
    (arg-id body)
    (begin
      (define new-arg-id (gensym arg-id))
      (fun new-arg-id
           (alpha-vary/env (hash-set env arg-id new-arg-id) body)))]
   [app
    (fun-expr arg-expr)
    (app
     (alpha-vary/env env fun-expr)
     (alpha-vary/env env arg-expr))]
   [tempty
    ()
    (tempty)]
   [tcons
    (first rest)
    (tcons
     (alpha-vary/env env first)
     (alpha-vary/env env rest))]
   [tfirst
    (e)
    (tfirst
     (alpha-vary/env env e))]
   [trest
    (e)
    (trest
     (alpha-vary/env env e))]
   [istempty
    (e)
    (istempty
     (alpha-vary/env env e))]))

(alpha-vary (parse complex-se))

(define (generate-constraints e-id e)
  (type-case
   Expr e
   [num
    (n)
    (list (eqc (t-var e-id) (t-num)))]
   [id
    (v)
    (list (eqc (t-var e-id) (t-var v)))]
   [bool
    (b)
    (list (eqc (t-var e-id) (t-bool)))]
   [bin-num-op
    (op lhs rhs)
    ;; (begin
    ;;   (define lhs-id (gensym 'bin-num-op-lhs))
    ;;   (define rhs-id (gensym 'bin-num-op-rhs))
    ;;   (append
    ;;    (list (eqc (t-var e-id) (t-num))
    ;;          (eqc (t-var lhs-id) (t-num))
    ;;          (eqc (t-var rhs-id) (t-num)))
    ;;    (generate-constraints lhs-id lhs)
    ;;    (generate-constraints rhs-id rhs)))
    (begin
      (append
       (list (eqc (t-var e-id) (t-num)))
       (generate-constraints e-id lhs)
       (generate-constraints e-id rhs)))]
   [iszero
    (e)
    (begin
      (define sub-e-id (gensym 'iszero-e))
      (append
       (list (eqc (t-var e-id) (t-bool))
             (eqc (t-var sub-e-id) (t-num)))
       (generate-constraints sub-e-id e)))]
   [bif
    (test then else)
    (begin
      (define test-id (gensym 'bif-test))
      (append
       (list (eqc (t-var test-id) (t-bool)))
       (generate-constraints e-id then)
       (generate-constraints e-id else)))]
   [with
    (bound-id bound-body body)
    (begin
      (append
       (generate-constraints bound-id bound-body)
       (generate-constraints e-id body)))]
   [rec-with
    (bound-id bound-body body)
    (begin
      (append
       (generate-constraints bound-id bound-body)
       (generate-constraints e-id body)))]
   [fun
    (arg-id body)
    (begin
      (define body-id (gensym 'fun-body))
      (append
       (list (eqc (t-var e-id)
                  (t-fun (t-var arg-id)
                         (t-var body-id))))
       (generate-constraints body-id body)))]
   [app
    (fun-expr arg-expr)
    (begin
      (define fun-id (gensym 'app-fun))
      (define arg-id (gensym 'app-arg))
      (append
       (list (eqc (t-var fun-id)
                  (t-fun (t-var arg-id)
                         (t-var e-id))))
       (generate-constraints fun-id fun-expr)
       (generate-constraints arg-id arg-expr)))]
   [tempty
    ()
    (list (eqc (t-var e-id)
               (t-list (t-var (gensym 'tempty)))))]
   [tcons
    (first rest)
    (begin
      (define first-id (gensym 'tcons-first))
      (append
       (list (eqc (t-var e-id)
                  (t-list (t-var first-id))))
       (generate-constraints first-id first)
       (generate-constraints e-id rest)))]
   [tfirst
    (e)
    (begin
      (define sub-e-id (gensym 'tfirst-e))
      (append
       (list (eqc (t-list (t-var e-id))
                  (t-var sub-e-id)))
       (generate-constraints sub-e-id e)))]
   [trest
    (e)
    (begin
      (append
       (list (eqc (t-list (t-var (gensym 'trest)))
                  (t-var e-id)))
       (generate-constraints e-id e)))]
   [istempty
    (e)
    (begin
      (define sub-e-id (gensym 'istempty-e))
      (append
       (list (eqc (t-list (t-var (gensym 'istempty)))
                  (t-var sub-e-id))
             (eqc (t-var e-id)
                  (t-bool)))
       (generate-constraints sub-e-id e)))]))

(generate-constraints
 (gensym 'top)
 (alpha-vary (parse complex-se)))

(define (unify cs)
  (unify/loop cs empty))

(define (unify/loop cs subst)
  (cond
    [(empty? cs)
     subst]
    [else
     (define top (first cs))
     (define next (rest cs))
     (define left (eqc-lhs top))
     (define right (eqc-rhs top))
     (cond
       ;; Case 1: same
       [(equal? left right)
        (unify/loop next subst)]
       ;; Case 2: var = something
       [(t-var? left)
        (when (occurs? left right)
          (error 'unify "occurs: ~e" top))
        (unify/loop (subst-cs left right next)
                    (cons top (subst-cs left right subst)))]
       ;; Case 3: something = var
       [(t-var? right)
        (unify/loop (cons (eqc right left) next)
                    subst)]
       ;; Case 4: list
       [(and (t-list? left)
             (t-list? right))
        (unify/loop (cons (eqc (t-list-elem left)
                               (t-list-elem right))
                          next)
                    subst)]
       ;; Case 4: fun
       [(and (t-fun? left)
             (t-fun? right))
        (unify/loop (list* (eqc (t-fun-arg left)
                                (t-fun-arg right))
                           (eqc (t-fun-result left)
                                (t-fun-result right))
                           next)
                    subst)]
       ;; Case 5
       [else
        (error 'unify "failed to unify: ~e" top)])]))

(define (occurs? maybe-in some-t)
  (or (equal? maybe-in some-t)
      (and (t-list? some-t)
           (occurs? maybe-in (t-list-elem some-t)))
      (and (t-fun? some-t)
           (or (occurs? maybe-in (t-fun-arg some-t))
               (occurs? maybe-in (t-fun-result some-t))))))

(define (subst-cs in out cs)
  (map (λ (c) (subst-c in out c)) cs))
(define (subst-c in out c)
  (eqc (subst-t in out (eqc-lhs c))
       (subst-t in out (eqc-rhs c))))
(define (subst-t in out t)
  (type-case
   Type t
   [t-num
    ()
    t]
   [t-bool
    ()
    t]
   [t-list
    (e)
    (t-list (subst-t in out e))]
   [t-fun
    (a r)
    (t-fun (subst-t in out a)
           (subst-t in out r))]
   [t-var
    (v)
    (if (equal? in t)
      out
      t)]))

(unify
 (generate-constraints
  (gensym 'top)
  (alpha-vary (parse complex-se))))

(define (infer-type e)
  (define top-label
    (gensym 'top))
  (eqc-rhs
   (findf
    (λ (c)
      (eq? top-label (t-var-v (eqc-lhs c))))
    (unify
     (generate-constraints
      top-label
      (alpha-vary e))))))

(infer-type (parse complex-se))

;; (infer-type (parse '(+ true 1)))
(infer-type (parse '(fun (x) x)))
(infer-type (parse '(fun (x) (+ 1 x))))

(infer-type (parse '(fun (a) (tfirst tempty))))
(infer-type (parse '(rec (f (fun (a) (f a))) f)))
(infer-type (parse '(rec (b b) (fun (a) b))))

;; (infer-type (parse '(fun (x) (tcons x x))))
;; (infer-type (parse '(rec (x (tcons x x)) x)))
;; (infer-type (parse '(fun (x) (x x))))
;; (infer-type (parse '(rec (f (fun (x) f)) f)))

;;;;;;;;;;;;;;;;;;;;

;; type=?/mapping : hash hash Type Type -> Bool
;; determines if types are equal modulo renaming
(define (type=?/mapping ht1 ht2 t1 t2)
  (define (teq? t1 t2)
    (type=?/mapping ht1 ht2 t1 t2))
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
                  ;; if t1 doesn't map to anything, it's the first
                  ;; time we're seeing it, so map it to t2
                  (hash-set! ht1 (t-var-v t1) (t-var-v t2))
                  (t-var-v t2)))]
             [define v2
               (hash-ref
                ht2 (t-var-v t2)
                (lambda ()
                  (hash-set! ht2 (t-var-v t2) (t-var-v t1))
                  (t-var-v t1)))])
            ;; we have to check both mappings, so that distinct
            ;; variables are kept distinct. i.e. a -> b should not be
            ;; isomorphic to c -> c under the one-way mapping a => c,
            ;; b => c.
            (and (symbol=? (t-var-v t2) v1)
                 (symbol=? (t-var-v t1) v2)))]
    [(and (Type? t1) (Type? t2)) false]
    [else (error 'type=? "either ~a or ~a is not a Type" t1 t2)]))

;; type=? Type -> Type -> Bool
;; signals an error if arguments are not variants of Type
(define ((type=? t1) t2)
  (or (type=?/mapping (make-hash) (make-hash) t1 t2)
      ;; Unfortunately, test/pred simply prints false; this helps us
      ;; see what t2 was.
      (error 'type=?
             "~s and ~a are not equal (modulo renaming)"
             t1 t2)))

(test/pred (t-var 'a)
           (type=? (t-var 'b)))
(test/pred (t-fun (t-var 'a) (t-var 'b))
           (type=? (t-fun (t-var (gensym)) (t-var (gensym)))))
(test/pred (t-fun (t-var 'a) (t-var 'b))
           (type=? (t-fun (t-var (gensym)) (t-var (gensym)))))
(test/exn ((type=? 34) 34) "not a Type")

;; constraint-list=? : Constraint list -> Constraint list -> Bool
;; signals an error if arguments are not variants of Constraint
(define ((constraint-list=? lc1) lc2)
  (define htlc1 (make-hash))
  (define htlc2 (make-hash))
  (or (andmap (lambda (c1 c2)
                (and
                 (type=?/mapping
                  htlc1 htlc2
                  (eqc-lhs c1) (eqc-lhs c2))
                 (type=?/mapping
                  htlc1 htlc2
                  (eqc-rhs c1) (eqc-rhs c2))))
              lc1 lc2)
      (error 'constraint-list=?
             "~s and ~a are not equal (modulo renaming)"
             lc1 lc2)))
