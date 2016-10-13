#lang plai
(module+ main
  ;; What is OO?

  ;; function : passive-data -> passive-data
  ;; passive-data = ... nums ... bools ...

  ;; sort : passive-data (list) X comparison-function -> passive-data
  ;; comparison-function : num X num -> bool

  ;; ACTIVE DATA

  (define (filter-expensive threshold l-o-ns)
    (filter (λ (x) (<= x threshold))
            ;; Inside of filter, "threshold" is inaccessible
            ;; but the lambda CAN access it
            l-o-ns))
  (test (filter-expensive 5 (list 1 2 3 4 5 6))
        (list 1 2 3 4 5))

  ;; ACTIVE DATA = behavior w/ (sealed data)

  (define (compose1 before after)
    ;; This fun is active
    (λ (x)
      (after (before x)))
    ;; It can access before and after but the context cannot
    )

  ;; OBJECTS are ACTIVE DATA
  ;; - have many questions vs just one (for functions)
  ;; - mutiple-entry-point function


  ;; C++: Posn *p = new Posn(3, 4)
  ;;         p = ptr to 8086
  ;; mem[8086] = ptr to 100
  ;; mem[8087] = 3
  ;; mem[8088] = 4
  ;;
  ;;       Posn *q = new Posn(8, 9)
  ;;         q = ptr to 8089
  ;; mem[8089] = ptr to 100
  ;; mem[8090] = 8
  ;; mem[8091] = 9
  ;;
  ;;  mem[100] = "Posn"
  ;;  mem[101] = ptr to get-x code
  ;;  mem[102] = ptr to distance-from-origin code
  ;;mem[get-x] = get p (meaning 8086), return mem[p+1]

  ;; p.get-x() ===>
  ;; *((*p)[1])(p)

  ;; posn_get_x() ==>
  ;; jump to 101

  (define (make-posn-object x y)
    (λ (me message)
      (cond
        [(eq? message 'distance-from-origin)
         (sqrt (+ (sqr x) (sqr y)))]
        [(eq? message 'get-x)
         x])))

  (define (dot obj message)
    (obj obj message))

  (define posn
    (make-posn-object 3 4))
  ;; posn = ClosureV( message-handling-code, Env(x -> 3, y -> 4) )

  #|
  posn (int x, int y) {
  distance-from-origin:
  return sqrt(sqr(x) + sqr(y));
  get-x:
  return x;
  }
  |#

  (test (dot posn 'distance-from-origin) 5)
  (test (dot posn 'get-x) 3)

  (define make-counter-object
    (let ()
      (define static-count 0) ; Static variable because outside of constructor

      ;; This lambda is the constructor/class
      (λ ()
        (define static-count-for-me static-count)
        (set! static-count (+ static-count 1))

        (define the-count 0) ; Instance variable because it is in the environment

        ;; Functions defined for the instance that are not messages are
        ;; private
        (define (reset!)
          (set! the-count 0))

        ;; Option 1: Mutation
        #;#;#;
        (define me #f)
        (set! me (λ (message) .....))
        me

        ;; Option 2: Pass me during calls
        ;; This lambda is the object
        (λ (me message)
          #;(define the-count 0) ; Local variable of the method
          (case message
            [(which-counter)
             static-count-for-me]
            [(more)
             (set! the-count (+ 1 the-count))
             (when (<= 4 the-count)
               (reset!))]
            [(how-many)
             (begin0 the-count
               (reset!))]
            [(lots-more)
             (λ (how-many-more)
               (for ([i (in-range how-many-more)])
                 (dot me 'more)))])))))

  (define awesomes (make-counter-object))
  (dot awesomes 'more)
  (dot awesomes 'more)
  (dot awesomes 'more)
  (test (dot awesomes 'how-many) 3)
  (test (dot awesomes 'how-many) 0)
  (dot awesomes 'more)
  (dot awesomes 'more)
  (dot awesomes 'more)
  (dot awesomes 'more)
  (test (dot awesomes 'how-many) 0)

  (define boos (make-counter-object))
  (test (dot boos 'which-counter) 1)
  (test (dot awesomes 'which-counter) 0)

  ((dot boos 'lots-more) 3)
  (test (dot boos 'how-many) 3)
  ((dot boos 'lots-more) 4)
  (test (dot boos 'how-many) 0)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Polymorphism
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define PI 3)

  (require racket/stxparam)
  (define-syntax-parameter me (λ (stx) (raise-syntax-error 'me "no" stx)))
  (define-syntax-rule
    (make-class #:parent parent
                [#:static static ...]
                #:args args
                [#:instance instance ...]
                [#:method name body] ...)
    (let ()
      ;; static
      static ...
      ;; constructor
      (λ args ; <- args
        (define my-parent parent)
        ;; instance
        instance ...
        ;; object
        (λ (this-me message)
          (syntax-parameterize ([me (make-rename-transformer #'this-me)])
            (case message
              [(name) body] ...
              [else
               (my-parent this-me message)]))))))

  (define make-shape-obj
    (make-class
     #:parent #f
     [#:static]
     #:args ()
     [#:instance]
     [#:method area
      (begin
        (error 'area "Must be implemented by child")
        (error 'area "Uninitialize abstract method call"))]
     [#:method double-area
      (* 2 (dot me 'area))]))

  (define make-rect-obj
    (make-class
     #:parent (make-shape-obj)
     [#:static]
     #:args (w h)
     [#:instance]
     [#:method area
      (* w h)]))

  (define make-circ-obj
    #;(make-class
       #:parent (make-shape-obj)
       [#:static]
       #:args (r)
       [#:instance]
       [#:method area
        (* PI r r)])
    (let ()
      ;; static

      ;; constructor
      (λ (r) ; <- args
        (define my-parent
          (make-shape-obj))
        ;; instance

        ;; object
        (λ (me message)
          (case message
            [(area)
             (* PI r r)]
            [else
             #;
             (dot my-parent message)
             ;; This is why we don't do the mutation to get this and
             ;; without it we get an error
             (my-parent me message)

             ;; Multiple inheritance...
             ;; Catch an error from above and then do this:
             #;
             (my-other-parent me message)
             ])))))

  (define rect (make-rect-obj 5 10))
  (define circ (make-circ-obj 4))

  (test (dot rect 'area) 50)
  (test (dot circ 'area) (* PI 16))

  (test (dot rect 'double-area) (* 2 50))
  (test (dot circ 'double-area) (* 2 (* PI 16)))

  ;; Prototype objects

  ;; Mixins
  (define (mixin some-parent)
    (λ args
      (make-class
       #:parent (apply some-parent args)
       [#:static ]
       #:args ()
       [#:instance]
       [#:method f 'x])))

  (mixin make-rect-obj)
  (mixin make-circ-obj)

  )
