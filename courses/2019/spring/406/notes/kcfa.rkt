#lang racket/base
(require racket/set
         racket/match
         racket/function
         racket/list)

;; A simple implementation of k-CFA.
;; Author: Matthew Might (translated by Jay McCarthy)
;; Site:   http://matt.might.net/

;; k-CFA is a well-known hierarchy of increasingly precise
;; control-flow analyses that approximate the solution to the
;; control-flow problem.

;; This program is a simple implementation of an
;; abstract-interpretion-based k-CFA for continuation-
;; passing-style lambda calculus (CPS).

;; Contrary to what one might expect, it does not use
;; constraint-solving.  Rather, k-CFA is implemented
;; as a small-step abstract interpreter.  In fact, it looks
;; suspiciously like an ordinary (if non-deterministic)
;; Scheme interpreter.

;; The analysis consists of exploring the reachable
;; parts of a graph of abstract machine states.

;; Once constructed, a composite store mapping addresses
;; to values is synthesized.

;; After that, the composite store is further summarized to
;; produce a mapping from variables to simple lambda terms.

;; The language over which the abstract interpreter operates is the
;; continuation-passing style lambda calculus (CPS):

;; exp  ::= (make-ref    <label> <var>)
;;       |  (make-lam    <label> (<var1> ... <varN>) <call>)
;; call ::= (make-call   <label> <exp0> <exp1> ... <expN>)

;; label = uninterned symbol

;; CPS is a popular intermediate form for functional compilation.
;; Its simplicity also means that it takes less code to construct
;; the abstract interpreter.

;; Helpers
(define empty-set (set))

; map-set : (a -> b) (set a) -> (set b)
(define (map-set f s)
  (for/fold ([ns empty-set])
            ([e (in-set s)])
    (set-add ns (f e))))

; take* is like take but allows n to be larger than (length l)
(define (take* l n)
  (for/list ([e (in-list l)]
             [i (in-naturals)]
             #:when (i . < . n))
    e))

;; Syntax.
(struct stx (label) #:prefab)
(struct exp stx () #:prefab)
(struct ref exp (var) #:prefab)
(struct lam exp (formals call) #:prefab)
(struct call stx (fun args) #:prefab)

;; Abstract state-space.

;; state ::= (make-state <call> <benv> <store> <time>)
(struct state (call benv store time) #:prefab)

;; benv = hash[var,addr]
;; A binding environment maps variables to addresses.
(define empty-benv (hasheq))

; benv-lookup : benv var -> addr
(define benv-lookup hash-ref)

; benv-extend : benv var addr -> benv
(define benv-extend hash-set)

; benv-extend* : benv list[var] list[addr] -> benv
(define (benv-extend* benv vars addrs)
  (for/fold ([benv benv])
            ([v (in-list vars)]
             [a (in-list addrs)])
    (benv-extend benv v a)))

;; store = hash[addr,d]
;; A store (or a heap/memory) maps address to denotable values.
(define empty-store (hasheq))

; store-lookup : store addr -> d
(define (store-lookup s a)
  (hash-ref s a d-bot))

; store-update : store addr d -> store
(define (store-update store addr value)
  (hash-update store addr
               (lambda (d) (d-join d value))
               d-bot))

; store-update* : store list[addr] list[d] -> store
(define (store-update* store addrs values)
  (for/fold ([store store])
            ([a (in-list addrs)]
             [v (in-list values)])
    (store-update store a v)))

; store-join : store store -> store
(define (store-join store1 store2)
  (for/fold ([new-store store1])
            ([(k v) (in-hash store2)])
    (store-update new-store k v)))

;; d = set[value]
;; An abstract denotable value is a set of possible values.
(define d-bot empty-set)

; d-join : d d -> d
(define d-join set-union)

;; value = clo
;; For pure CPS, closures are the only kind of value.

;; clo ::= (make-closure <lambda> <benv>)
;; Closures pair a lambda term with a binding environment that
;; determinse the value of its free variables.
(struct closure (lam benv) #:prefab)

;; addr = bind
;; Addresses can point to values in the store.
;; In pure CPS, the only kind of addresses are bindings.

;; bind ::= (make-binding <var> <time>)
;; A binding is minted each time a variable gets bound to a value.
(struct binding (var time) #:prefab)

;; time = (listof label)
;; In k-CFA, time is a bounded memory of program history.
;; In particular, it is the last k call sites through which
;; the program has traversed.
(define time-zero empty)

;; k-CFA parameters

;; Change these to alter the behavior of the analysis.

; k : natural
(define k (make-parameter 1))

; tick : call time -> time
(define (tick call time)
  (take* (list* (stx-label call) time) (k)))

; alloc : time -> var -> addr
(define ((alloc time) var) (binding var time))

;; k-CFA abstract interpreter

; atom-eval : benv store -> exp -> d
(define (atom-eval benv store)
  (match-lambda
    [(struct ref (_ var))
     (store-lookup store (benv-lookup benv var))]
    [(? lam? lam)
     (set (closure lam benv))]))

; next : state -> set[state]
(define (next st)
  (match-define (struct state (c benv store time)) st)
  (define time* (tick c time))
  (match c
    [(struct call (_ f args))
     (define procs ((atom-eval benv store) f))
     (define params (map (atom-eval benv store) args))
     (for/list ([proc (in-set procs)])
       (match proc
         [(struct closure ((struct lam (_ formals call*)) benv*))
          (define bindings (map (alloc time*) formals))
          (define benv**   (benv-extend* benv* formals bindings))
          (define store*   (store-update* store bindings params))
          (state call* benv** store* time*)]))]
    [_
     empty]))

;; State-space exploration.

; exlore : set[state] list[state] -> set[state]
(define (explore seen todo)
  (match todo
    [(list)
     seen]
    [(list-rest (? (curry set-member? seen)) todo)
     (explore seen todo)]
    [(list-rest st0 todo)
     (define succs (next st0))
     (explore (set-add seen st0)
              (append succs todo))]))

;; User Interface

; summarize : set[state] -> store
(define (summarize states)
  (for/fold ([store empty-store])
            ([state (in-set states)])
    (store-join (state-store state) store)))

(define empty-mono-store (hasheq))

; monovariant-store : store -> alist[var,exp]
(define (monovariant-store store)
  (for/fold ([mono-store empty-mono-store])
            ([(b vs) (in-hash store)])
    (hash-update mono-store
                 (binding-var b)
                 (lambda (b-vs)
                   (set-union
                    b-vs
                    (map-set monovariant-value vs)))
                 empty-set)))

; monovariant-value : val -> exp
(define monovariant-value
  (match-lambda
    [(? closure? c) (closure-lam c)]))

; analyze : exp -> mono-summary
(define (analyze exp)
  (define init-state (state exp empty-benv empty-store time-zero))
  (define states (explore empty-set (list init-state)))
  (define summary (summarize states))
  (define mono-summary (monovariant-store summary))
  mono-summary)

; print-mono-summary : mono-summary -> void
(define (print-mono-summary ms)
  (for ([(i vs) (in-hash ms)])
    (printf "~a:~n" i)
    (for ([v (in-set vs)])
      (printf "\t~S~n" v))
    (printf "~n")))

;; Helper functions for constructing syntax trees:
(define new-label gensym)

(define (make-ref* var)
  (ref (new-label) var))

(define (make-lambda* formals call)
  (lam (new-label) formals call))

(define (make-call* fun . args)
  (call (new-label) fun args))

(define (make-let* var exp call)
  (make-call* (make-lambda* (list var) call) exp))


;; The Standard Example
;;
;; In direct-style:
;;
;; (let* ((id (lambda (x) x))
;;        (a  (id (lambda (z) (halt z))))
;;        (b  (id (lambda (y) (halt y)))))
;;   (halt b))
(define standard-example
  (make-let* 'id (make-lambda* '(x k) (make-call* (make-ref* 'k) (make-ref* 'x)))
             (make-call* (make-ref* 'id)
                         (make-lambda* '(z) (make-ref* 'z))
                         (make-lambda* '(a)
                                       (make-call* (make-ref* 'id)
                                                   (make-lambda* '(y) (make-ref* 'y))
                                                   (make-lambda* '(b)
                                                                 (make-ref* 'b)))))))

(module+ main
  (for ([a-k (in-list (list 0 1 2))])
    (printf "K = ~a~n" a-k)
    (parameterize ([k a-k])
      (print-mono-summary
       (analyze standard-example)))))
