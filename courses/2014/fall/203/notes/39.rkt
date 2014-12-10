#lang racket/base
(require racket/match
         racket/list)

;; Monad - Endofunctor in the category of monoids
(struct monad (return bind))
(struct monad-plus monad (fail))
;; C is pronounced Monad

;; p : C C<Num> C<Num> -> C<Num>
(define (p a-monad Cx Cy)
  (match-define (monad return bind) a-monad)
  (bind Cx
        (λ (x)
          (bind Cy
                (λ (y)
                  (define z (+ x y))
                  (define a (* z z))
                  (return a))))))

(define vals
  ;; C<A> = A
  (monad
   ;; return : A -> C<A>
   ;; return : A -> A
   (λ (val)
     val)
   ;; bind : C<A> -> (A -> C<B>) -> C<B>
   ;; bind : A -> (A -> B) -> B
   (λ (C f)
     (f C))))

(define lists-first
  ;; C<A> = List<A>
  (monad
   ;; return : A -> C<A>
   ;; return : A -> List<A>
   (λ (val)
     (list val))
   ;; bind : C<A> -> (A -> C<B>) -> C<B>
   ;; bind : List<A> -> (A -> List<B>) -> List<B>
   (λ (C f)
     (f (first C)))))

(define lists
  ;; C<A> = List<A>
  (monad
   ;; return : A -> C<A>
   ;; return : A -> List<A>
   (λ (val)
     (list val))
   ;; bind : C<A> -> (A -> C<B>) -> C<B>
   ;; bind : List<A> -> (A -> List<B>) -> List<B>
   (λ (C f)
     ;; map : List<A> -> (A -> B) -> List<B>
     ;; thus this is List<List<B>>
     (append* (map f C)))))

(p vals 5 10)
(p lists-first '(5 6 7 8) '(10 11 13))
;; errors
;; (p lists-first '(5 6 7 8) '())

(p lists '(5 6 7 8) '(10 11 13))

(define maybe
  ;; C<A> = Maybe<A>
  ;; Maybe<A> = A | #f
  (monad-plus
   ;; return : A -> C<A>
   ;; return : A -> List<A>
   (λ (val)
     val)
   ;; bind : C<A> -> (A -> C<B>) -> C<B>
   ;; bind : List<A> -> (A -> List<B>) -> List<B>
   (λ (C f)
     (if C (f C) #f))
   (λ ()
     #f)))

(p maybe 5 10)
(p maybe 5 #f)
(p maybe #f 10)

(define (p2 monad Cx Cy)
  (match-define (monad-plus return bind fail)
                monad)
  (bind Cx
        (λ (x)
          (bind Cy
                (λ (y)
                  (define z (+ x y))
                  (cond
                   [(> z 20)
                    (printf
                     "Failed on ~v,~v\n"
                     x y)
                    (fail)]
                   [else
                    (define a (* z z))
                    (return a)]))))))

(p2 maybe 5 10)
(p2 maybe 5 #f)
(p2 maybe #f 10)
(p2 maybe 10 11)

(define exnM
  ;; C<A> = exnM<A>
  ;; exnM<A> = A | String
  (monad
   ;; return : A -> C<A>
   ;; return : A -> List<A>
   (λ (val)
     val)
   ;; bind : C<A> -> (A -> C<B>) -> C<B>
   ;; bind : List<A> -> (A -> List<B>) -> List<B>
   (λ (C f)
     (if (string? C) C (f C)))))

(p exnM 5 10)
(p exnM 5 "Couldn't find y")
(p exnM "Couldn't find x" 10)

(define nonDetM
  (monad-plus
   (λ (val)
     (list val))
   (λ (C f)
     (or
      (for/or ([option (in-list C)])
        (define r (f option))
        (if (empty? r)
            #f
            r))
      empty))
   (λ ()
     empty)))

(p2 nonDetM
    '(10 7 3)
    '(19 17))

(struct pl:return (x) #:transparent)
(struct pl:bind (a f) #:transparent)
(define programmingLanguageM
  (monad pl:return pl:bind))

(define (interp prog)
  (match prog
    [(pl:return x)
     x]
    [(pl:bind a f)
     (define av (interp a))
     (interp (f av))]))

(p vals 10 11)

(define p-as-prog
  (p programmingLanguageM
     (pl:return 10)
     (pl:return 11)))
p-as-prog
(interp p-as-prog)

;; Haskell
;; - no mutation
;;   there is no list.first = 6
;; - f(7) = 9 always

(map (λ (x) (* x x))
     (map (λ (y) (+ y y))
          '(1 2 3 4)))
(map (λ (x) (* x x))
     ;; -> This is extra junk
     '(2 4 6 8))
(map (λ (y)
       (define x (+ y y))
       (* x x))
     '(1 2 3 4))

(define C 0)
(map (λ (x) 
       (+ (* x x) C))
     (map (λ (y)
            (set! C (+ C y))
            (+ y y))
          '(1 2 3 4)))
(set! C 0)
(map (λ (y)
       (set! C (+ C y))
       (define x (+ y y))
       (+ (* x x) C))
     '(1 2 3 4))

(define (g x)
  (* x x x x x x x x))
(second (list (g 5) (g 7) (g 9)))

;; no effects AT ALL in Haskell

;;    Java:  main : String[] -> void (prints stuff)
;;       C:  main : int char** -> int (prints stuff)
;; Haskell:  main : void (doesn't print stuff)

;; IO<A> is a Monad<A>
;;   where there are more functions, such as...
;; print : String -> IO<void>

;; Haskell:  main : IO int
;; FORTRAN:  IO int -> int (prints stuff) 
