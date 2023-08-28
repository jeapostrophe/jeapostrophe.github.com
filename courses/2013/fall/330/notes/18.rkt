#lang plai
(print-only-errors #t)
(halt-on-errors #t)

;; Object oriented
;; ... classes -

;; ... structures w/ associated functions [same as class?]

;; ... inheritance - use a class as a "base" of another, new one
;; includes everything from the base (thing = data + functions?);
;; about sharing code/data

;; What's the difrferen between class and object?
;; - Class is a blueprint and object is something built by it

;; Can an object exist without a class?

;; In JSON.. JavaScript Object Notation... JS!
;; { foo: 1 }
;; { foo: 1, add : function () {  return 1; }  }

;; Structure w/ functions - data + code with names

;; p.x p.y p.add()

;; Desugar : Source syntax -> core syntax
(define-syntax-rule
  (object [piece value]
          ...)
  ;; becomes
  (λ (asked-for-piece)
    (cond
      [(eq? asked-for-piece 'piece)
       value]
      ...
      [else
       (error 'object "message not understood: ~e" asked-for-piece)])))

(define-syntax-rule
  (dot some-object some-piece)
  ;; becomes
  (some-object 'some-piece))

(define-syntax-rule
  (dot-computed some-object some-piece-expr)
  ;; becomes
  (some-object some-piece-expr))

;; Object : Names -> Values
(define o1
  (object [x 5]
          [y 6]
          [sum
           (λ (a b)
             (+ a b))]))
(define o1-raw
  (λ (asked-for-piece)
    (cond
      [(eq? asked-for-piece 'x)
       5]
      [(eq? asked-for-piece 'y)
       6]
      [(eq? asked-for-piece 'sum)
       (λ (a b)
         (+ a b))]
      [else
       (error 'object "message not understood")])))

;; Who invented OO? Alan Kay ... the Actor

(test (o1 'x) 5)
(test (o1 'y) 6)
(test ((o1 'sum) 2 3) 5)

(test (dot o1 x) 5)
(test (dot o1 y) 6)
(test ((dot o1 sum) 2 3) 5)

(test (o1-raw 'x) 5)
(test (o1-raw 'y) 6)
(test ((o1-raw 'sum) 2 3) 5)

(test (dot o1-raw x) 5)
(test (dot o1-raw y) 6)
(test ((dot o1-raw sum) 2 3) 5)

;; Java has names written in code:
;;  o1.x
;; Javascript, Ruby, PHP has names computed in code:
;;  o1.(string->symbol "x")
(test/exn (dot o1 (+ 1 2)) "'(+ 1 2)")
(test/exn (dot-computed o1 (+ 1 2)) "3")

;; make-one-of-these is a constructor
;; make-one-of-these is a "class"

;; Class : Stuff -> Object

(define make-one-of-these
  (λ (the-x the-y)
    (object [x the-x]
            [y the-y]
            [sum
             (λ (a b)
               (+ a b))])))

(define (make-one-of-THESE x)
  (make-one-of-these x x))

(define o2 (make-one-of-these 7 8))

(test (dot o2 x) 7)
(test (dot o2 y) 8)

(define posn%
  (λ (the-x the-y)
    (object [class posn%]
            [x the-x]
            [y the-y]
            [sum
             (λ (a b)
               (+ a b))])))

(define o3 (posn% 7 8))

(test (dot o3 class) posn%)
(test (dot o3 x) 7)
(test (dot o3 y) 8)

(define posn/private%
  (λ (the-x the-y)
    (let ([private-zone #f])
      (object [x the-x]
              [set-x
               (λ (new-x)
                 (set! the-x new-x))]
              [y the-y]
              [sum
               (λ (a b)
                 (set! private-zone
                       (not private-zone))
                 (if private-zone
                   (+ a b)
                   (- a b)))]))))

(define o3-0 (posn/private% 7 8))

(test ((dot o3-0 sum) 1 2) 3)
(test ((dot o3-0 sum) 1 2) -1)

(test (dot o3-0 x) 7)
(test ((dot o3-0 set-x) 10) (void))
(test (dot o3-0 x) 10)

(define posn/origin%
  (let ([origin-x 0]
        [origin-y 0])
    (λ (the-x the-y)
      (object [x the-x]
              [y the-y]
              [set-origin-x
               (λ (x)
                 (set! origin-x x))]
              [origin
               (λ ()
                 (format "~a,~a"
                         origin-x
                         origin-y))]))))

(define o4 (posn/origin% 7 8))
(define o5 (posn/origin% 8 9))

(test ((dot o4 origin)) ((dot o5 origin)))
(test ((dot o5 origin)) "0,0")
(test ((dot o4 set-origin-x) 10) (void))
(test ((dot o5 origin)) "10,0")

(define-syntax-rule
  (define-class class-name%
    (static [static-f static-v]
            ...)
    (constructor (constructor-arg ...))
    (fields [field-name field-ivalue]
            ...)
    (method (method-name method-arg ...)
      method-body ...)
    ...)
  ;; becomes
  (define class-name%
    (let ([static-f static-v]
          ...)
      (λ (constructor-arg ...)
        (object [field-name field-ivalue]
                ...
                [method-name
                 (λ (method-arg ...)
                   method-body ...)]
                ...)))))

(define-class posn/origin-with-class%
  (static [origin-x 0]
          [origin-y 0])
  (constructor (the-x the-y))
  (fields [x the-x]
          [y the-y])
  (method (set-origin-x x)
    (set! origin-x x))
  (method (origin)
    (format "~a,~a"
            origin-x
            origin-y)))

(define o4-with-c (posn/origin-with-class% 7 8))
(define o5-with-c (posn/origin-with-class% 8 9))

(test ((dot o4-with-c origin)) ((dot o5-with-c origin)))
(test ((dot o5-with-c origin)) "0,0")
(test ((dot o4-with-c set-origin-x) 10) (void))
(test ((dot o5-with-c origin)) "10,0")

(define posn/origin2%
  (let ([origin-x 0]
        [origin-y 0])
    (λ (the-x the-y)
      (object [x the-x]
              [y the-y]
              [set-origin-x
               (λ (self x)
                 (set! origin-x x))]
              [set-origin-y
               (λ (self y)
                 (set! origin-y y))]
              [set-origin
               (λ (self x y)
                 ;; (set! origin-x x)
                 ;; (set! origin-y y)
                 ((dot self set-origin-x) self x)
                 ((dot self set-origin-y) self y))]
              [origin
               (λ (self)
                 (format "~a,~a"
                         origin-x
                         origin-y))]))))

(define o6 (posn/origin2% 5 6))
(test ((dot o6 origin) o6) "0,0")
(test ((dot o6 set-origin) o6 2 2) (void))
(test ((dot o6 origin) o6) "2,2")

(define-syntax-rule
  (dot-self obj msg arg ...)
  ;; becomes
  ((dot obj msg) obj arg ...))

(define posn/origin2-less-tedious%
  (let ([origin-x 0]
        [origin-y 0])
    (λ (the-x the-y)
      (object [x the-x]
              [y the-y]
              [set-origin-x
               (λ (self x)
                 (set! origin-x x))]
              [set-origin-y
               (λ (self y)
                 (set! origin-y y))]
              [set-origin
               (λ (self x y)
                 ;; (set! origin-x x)
                 ;; (set! origin-y y)
                 (dot-self self set-origin-x x)
                 (dot-self self set-origin-y y))]
              [origin
               (λ (self)
                 (format "~a,~a"
                         origin-x
                         origin-y))]))))

(define o7 (posn/origin2-less-tedious% 5 6))
(test (dot-self o7 origin) "0,0")
(test (dot-self o7 set-origin 2 2) (void))
(test (dot-self o7 origin) "2,2")

(define-syntax-rule
  (object-with-self self-id
                    [field val]
                    ...)
  ;; becomes
  (let ([self-id #f])
    (set! self-id
          (object [field val]
                  ...))
    self-id))

(define posn/origin3%
  (let ([origin-x 0]
        [origin-y 0])
    (λ (the-x the-y)
      (object-with-self
       self
       [x the-x]
       [y the-y]
       [set-origin-x
        (λ (x)
          (set! origin-x x))]
       [set-origin-y
        (λ (y)
          (set! origin-y y))]
       [set-origin
        (λ (x y)
          ;; (set! origin-x x)
          ;; (set! origin-y y)
          ((dot self set-origin-x) x)
          ((dot self set-origin-y) y))]
       [origin
        (λ ()
          (format "~a,~a"
                  origin-x
                  origin-y))]))))

(define o8 (posn/origin3% 5 6))
(test ((dot o8 origin)) "0,0")
(test ((dot o8 set-origin) 2 2) (void))
(test ((dot o8 origin)) "2,2")

;;;; Monday

;; (define-interface tree<%>
;;   (sum))

(define-class leaf% ;; must be tree<%>
  (static)
  (constructor (val))
  (fields)
  (method (sum)
    val))

(define-class branch%
  (static)
  (constructor (lhs rhs))
  (fields)
  (method (sum)
    (+ ((dot lhs sum))
       ((dot rhs sum)))))

(define-class binary-tree%
  (static)
  (constructor ())
  (fields)
  (method (sum)
    42))

(define l1 (leaf% 5))
(define l2 (leaf% 6))
(define b1 (branch% l1 l2))
(define b2 (branch% b1 l2))

(test ((dot b2 sum)) 17)
(test ((dot (branch% (binary-tree%) b2) sum)) (+ 42 17))

;; OO easy to add kind of tree... we only edited the class definition

;; Dynamic Dispatch

(define-type Tree
  [leaf (val number?)]
  [branch (lhs Tree?) (rhs Tree?)])

(define (sum t)
  (type-case
   Tree t
   [leaf
    (val)
    val]
   [branch
    (lhs rhs)
    (+ (sum lhs) (sum rhs))]))

(test (sum (branch (branch (leaf 5) (leaf 6))
                   (leaf 6)))
      17)

;; Struct adding a kind = changing every function

;; The "Expression Problem"
;; If you have N kinds and M functions

;; When you add a kind, OO changes 1 thing but Struct changes M

;; When you add a function, OO changes N things but Struct changes 1

(define-syntax-rule
  (object/parent
   parent
   [piece value]
   ...)
  ;; becomes
  (λ (asked-for-piece)
    (cond
      [(eq? asked-for-piece 'piece)
       value]
      ...
      [else
       (dot-computed parent asked-for-piece)])))

(define-syntax-rule
  (object-with-self/parent
   self-id parent
   [field val]
   ...)
  ;; becomes
  (let ([self-id #f])
    (set! self-id
          (object/parent
           parent
           [field val]
           ...))
    self-id))

(define (leaf-ish% val)
  (object-with-self
   self
   [secret (λ () 0)]
   [sum (λ () (+ val ((dot self secret))))]))

(define (leaf/size% val)
  (object-with-self/parent
   self
   (leaf-ish% val)
   ;; Does not work beacuse of set! style
   ;; The "Python" style doesn't have this problem
   ;; because self is a dynamic value
   ;;
   ;; The "Open Recursion" problem
   [secret 
    (λ ()
      1)]
   [size
    (λ ()
      1)]
   [sizish
    (λ ()
      ((dot self sum)))]))

;; Should parent be an object or a class?

;; When the parent is an object, we say the object system is "prototype" based... comes from Self and JS implements it

(define (branch/size% lhs rhs)
  (object-with-self/parent
   self
   ;; (leaf% 42)
   (branch% rhs lhs)
   [size
    (λ ()
      (+ 1
         ((dot lhs size))
         ((dot rhs size))))]
   [sizish
    (λ ()
      (+ 1
         ((dot lhs sizish))
         ((dot rhs sizish))))]))

;; Mixin : Class -> Class
(define (thing/size% parent lhs rhs)
  (object-with-self/parent
   self
   (parent lhs rhs)
   [size
    (λ ()
      (+ 1
         ((dot lhs size))
         ((dot rhs size))))]))

(define l1/size (leaf/size% 5))
(define l2/size (leaf/size% 6))
(define b1/size (branch/size% l1/size l2/size))
(define b2/size (branch/size% b1/size l2/size))

(test ((dot b2/size sum)) 17)
(test ((dot b2/size size)) 5)
(test ((dot b2/size sizish)) 19)

(define b52 (thing/size% (λ (lhs rhs) rhs) b1/size b2/size))
(test ((dot b52 size)) 9)
(test ((dot b52 sum)) 17)

;; M provides function m
;; N needs function m and provides n
;; User wants m and n
;; But you want m to provided by something with N already

;; Mixin M, N
;; Base C

;; M1(N(M2(C)))
;; The names of M2 conflict with the names of M1

;; Trait ... is like a mixin but is just "pieces" not parent
