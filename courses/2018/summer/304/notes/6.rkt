#lang racket/base
(require racket/match
         racket/list
         racket/set)
(module+ test
  (require racket/pretty))

;; Regexp
(struct rx () #:transparent)
(struct ϵ rx ()  #:transparent)
(struct ∅ rx ()  #:transparent)
(struct :char rx (c)  #:transparent)
(struct ∪ rx (x y)  #:transparent)
(struct ∘ rx (x y)  #:transparent)
(struct :star rx (x)  #:transparent)

;; DFA
(struct DFA (Q q0 δ F) #:transparent)

;; NFA
(struct NFA (Q q0 δ F) #:transparent)

(define (rx-compile r)
  (match r
    [(ϵ) (NFA (set 'ϵ) 'ϵ (hash) (set 'ϵ))]
    [(∅) (NFA (set '∅) '∅ (hash) (set))]
    [(:char c)
     (NFA (set 'c-start 'c-end)
          'c-start
          (hash (cons 'c-start c) (set 'c-end))
          (set 'c-end))]
    [(∪ r1 r2)
     (match-define (NFA Q1 q0.1 δ1 F1) (rx-compile r1))
     (match-define (NFA Q2 q0.2 δ2 F2) (rx-compile r2))
     (NFA (set-union (set '∪-start '∪-end)
                     (set-cons '∪-left Q1)
                     (set-cons '∪-right Q2))
          '∪-start
          (delta-merge
           (hash (cons '∪-start 'ϵ)
                 (set (cons '∪-left q0.1)
                      (cons '∪-right q0.2)))
           (delta-cons '∪-left δ1)
           (delta-cons '∪-right δ2)
           (for/hash ([qf1 (in-set F1)])
             (values (cons (cons '∪-left qf1) 'ϵ) (set '∪-end)))
           (for/hash ([qf2 (in-set F2)])
             (values (cons (cons '∪-right qf2) 'ϵ) (set '∪-end))))
          (set '∪-end))]))

(define (set-cons sym Q)
  (for/set ([qi (in-set Q)])
    (cons sym qi)))

(define (delta-cons sym δ)
  (for/hash ([(qi*c dst) (in-hash δ)])
    (match-define (cons qi c) qi*c)
    (values (cons (cons sym qi) c)
            (for/set ([d (in-set dst)])
              (cons sym d)))))

(define (delta-merge2 δ δi)
  (for/fold ([δ δ]) ([(qi*c dst) (in-hash δi)])
    (hash-set δ qi*c dst)))

(define (delta-merge δ0 . δs)
  (for/fold ([δ δ0]) ([δi (in-list δs)])
    (delta-merge2 δ δi)))

(define (power-set Qn)
  (list->set (map list->set (combinations (set->list Qn)))))

(define (NFA->DFA n)
  (match-define (NFA Qn q0n δn Fn) n)
  (define Qd (power-set Qn))
  (define (E Q)
    (define Qp
      (for/fold ([Q Q]) ([qi (in-set Q)])
        (set-union Q (hash-ref δn (cons qi 'ϵ) (set)))))
    (if (set=? Qp Q)
      Q
      (E Qp)))
  (define q0d (E (set q0n)))
  (define Fd
    (list->set
     (filter (λ (qi) (not (set-empty? (set-intersect qi Fn))))
             (set->list Qd))))
  (define δd
    (for*/hash  ([qi (in-set Qd)]
                 ;; XXX fix me!
                 [c (in-list '(#\2 #\0 #\1 ϵ))])
      (values (cons qi c)
              (E
               (for/fold ([s (set)]) ([qd (in-set qi)])
                 (set-union s (hash-ref δn (cons qd c) (set))))))))

  (DFA Qd q0d δd Fd))

(define (in-DFA? d w)
  (match-define (DFA Q q0 δ F) d)
  (set-member?
   F
   (for/fold ([qi q0]) ([c (in-string w)])
     (hash-ref δ (cons qi c) #f))))

(module+ test
  (define the-dfa (NFA->DFA
                   (rx-compile (∪ (:char #\2)
                                  (∪ (:char #\0)
                                     (:char #\1))))))
  #;(pretty-print the-dfa)
  (in-DFA? the-dfa "0")
  (in-DFA? the-dfa "1")
  (in-DFA? the-dfa "2")
  (in-DFA? the-dfa "3"))
