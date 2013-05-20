#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Efficient Forth in 85 Lines of Racket}
@categories["Racket" "Forth" "Macros"]

@link["http://en.wikipedia.org/wiki/Forth_(programming_language)"]{Forth}
is an interesting programming language in the @tech{concatenative}
family. It relies crucially on a data stack, rather than functions
with their arguments and return values, as a means of communication.

Last week I wondered, "Could I implement a Forth-like system in
Racket, with interoperablitly between the two, with macros?" This post
presents
@link["https://github.com/jeapostrophe/exp/tree/master/rorth"]{the
system}.

@(the-jump)

@section{Basics}

In Racket, when you want to add two numbers, you pick the numbers and
deliver them to a function that does the addition and returns the
result:

@chunk[<ex1>
       (check-equal? (+ 1 2) 3)]

In most languages, functions are restricted to returning a single
value, like @racket[3] in this example. However, in Racket we do not
@emph{restrict the arity of continuations} and allow you to return
many values to the current continuation and create a context that
expects many results:

@chunk[<ex2/cc>
       (define (f a b)
         (let/cc k
           (k a b (+ a b))))
       (define-values (first-result second-result third-result)
         (f 1 2))
       (check-equal? first-result 1)
       (check-equal? second-result 2)
       (check-equal? third-result 3)]

It is awkward to think about n-ary continuations as the reason why
multiple return values makes sense, so in Racket we have a function
called @racket[values] that abstracts multiple return:

@chunk[<ex2/vs>
       (define (f/values a b)
         (values a b (+ a b)))
       (define-values (first-result second-result third-result)
         (f/values 1 2))
       (check-equal? first-result 1)
       (check-equal? second-result 2)
       (check-equal? third-result 3)]

Now, let's rewrite these programs in Forth.

In Forth, rather than than picking the numbers and @emph{delivering}
them to the function, you put the numbers in a place the function can
get to them. Similarly, rather than the function returning the answer
@emph{to you}, it places the answer in same shared location where the
rest of your program can do something with them.

This shared location is @emph{the data stack} and is the fundamental
thing that Forth programs revolve around. A test case for a Forth
program is a triple of (1) an input stack, (2) a program, and (3) a
result stack.

@chunk[<fex1>
       (check-forth () (1 2 :+) (3))]

In this example, simply by writing a number, like @racket[2], we are
placing it on the stack, and by simply writing a function like
@racket[:+], we are jumping to it where it can do its work.

In some sense it more natural to write the second example, because we
can more directly place multiple "answers" on the stack. On the other
hand, it is more complicated because we have to explicitly copy
information that we don't want to destroy. In Forth, you can do this
with functions like @racket[:dup] that @emph{dup}licate the thing on
top of the stack and @racket[:over] that duplicate the thing just
underneath that.

The example looks like this:

@chunk[<fex2>
       (define/forth :f
         :over :over :+)
       (check-forth () (1 2 :f) (1 2 3))]

@section{Specification}

We would like to enable this sort of programming in Racket, but with
the additional feature that Racket code should be able to seamlessly
call Forth code, @emph{provided we specify how many stack locations
the Forth code touches}. For example, we wouldn't be able to call
@racket[:f], because it is not obvious that it expects two things on
the stack and adds one. But, we could specify that and then call it:

@chunk[<fex2/spec>
       (define/forth :f (2 -- 3)
         :over :over :+)
       (check-forth () (1 2 :f) (1 2 3))
       (define-values (first-result second-result third-result)
         (:f 1 2))
       (check-equal? first-result 1)
       (check-equal? second-result 2)
       (check-equal? third-result 3)]

Similarly, we could give it a different specification that only shows
the new value, but that would only affect the Racket interface:

@chunk[<fex2/spec2>
       (define/forth :f (2 -- 1)
         :over :over :+)
       (check-forth () (1 2 :f) (1 2 3))
       (check-equal? (:f 1 2) 3)]

Thus, our specification is:
@itemlist[#:style 'ordered

@item{We must be able to define functions in Forth that are callable
from Forth, always.}

@item{We must be able to give functions stack effect annotations to
enable them to be called from Racket.}

@item{We must be able to @emph{lift} Racket functions to Forth so they
are oblivious to the stack, like turning @racket[+] into @racket[:+].}

@item{We must be able to @emph{lower} Racket functions to Forth so
they can directly affect the stack, like writing @racket[:over].}

@item{We must be able to enter Forth from Racket arbitrarily, such as
to write testing forms like @racket[check-forth].}
]

@section{Implementation: Data and Tests}

The first thing we'll do is decide to represent the stack as a normal
Racket list. Next, we'll represent Forth functions as special data
structure that holds a function that accepts a stack and returns a
stack:

@chunk[<forth-data>
       (struct forth-word (stack-transformer))]

For example, we could represent @racket[:dup] as:

@chunk[<raw-dup>
       (define :dup
         (forth-word
          (λ (stack)
            (list* (first stack) stack))))]

Although, we'll make definition of things like this easier in a
moment.

Next, we'll write a macro to compose Forth words, calling them as we
see them or pushing data when it is not a Forth word:

@chunk[<forth>
       (define (maybe-forth-word e stk)
         (if (forth-word? e)
           ((forth-word-stack-transformer e) stk)
           (list* e stk)))

       (define-syntax (forth stx)
         (syntax-parse stx
           [(_ #:stack stk)
            (syntax/loc stx stk)]
           [(_ #:stack stk e m ...)
            (syntax/loc stx (forth #:stack (maybe-forth-word e stk) m ...))]
           [(_ (~and (~not #:stack) e1) e ...)
            (syntax/loc stx (forth #:stack empty e1 e ...))]))]

This macro might look a little meaty, but it just left-associates
until it reaches the end, whereupon it returns the stack, and contains
an initialization case for the first call.

This lets use write code like:

@chunk[<dup-test>
       (check-equal?
        (forth 1 :dup)
        (list 1 1))]

However, using this directly in tests is a little awkward, because the
stack is store oppositely from our intuitions:

@chunk[<dup-test2>
       (check-equal?
        (forth 2 1 :dup)
        (list 1 1 2))]

Thus, it would be nice to have a macro that allows us to write the
stack in the "natural order", with the top on the right:

@chunk[<dup-test3>
       (check-forth () (2 1 :dup) (2 1 1))]

There's a natural way to write this:

@chunk[<check-forth1>
       (define-syntax-rule
         (check-forth (before ...) (word ...) (after ...))
         (check-equal? (forth #:stack (reverse '(before ...))
                              word ...)
                       (reverse '(after ...))))]

However, I think it is ugly to have to @racket[reverse] the before and
after data during evaluation of the program. So I like this version:

@chunk[<check-forth2>
       (define-syntax (syntax-reverse stx)
         (syntax-case stx ()
           [(_ (d ...))
            (with-syntax ([(dr ...) (reverse (syntax->list #'(d ...)))])
              (syntax/loc stx
                '(dr ...)))]))
       
       (define-syntax-rule
         (check-forth (before ...) (word ...) (after ...))
         (check-equal? (forth #:stack (syntax-reverse (before ...))
                              word ...)
                       (syntax-reverse (after ...))))]

@section{Implementation: Definitions}

Next, let's revisit the definition of Forth functions. It is ugly to
directly expose the @racket[forth-word] data structure. Instead, we
want to support the following notation:

@chunk[<forth-defns>
       (define/forth :dup (1 -- 2)
         #:lower stack
         (list* (first stack) stack))
       (define/forth :over (2 -- 3)
         #:lower stack
         (list* (second stack) stack))
       (define/forth :swap (2 -- 2)
         #:lower stack
         (match-define (list* a b more) stack)
         (list* b a more))

       (define/forth :+ (2 -- 1)
         #:lift +)
       (define/forth :* (2 -- 1)
         #:lift *)

       (define/forth :squared (1 -- 1)
         :dup :*)
       (define/forth :sum-of-squares (2 -- 1)
         :squared :swap :squared :+)]

And, in fact, one additional thing is that we don't really want to
force Forth functions to have names, so we'll really implement
@racket[forthda] as the core:

@chunk[<forthda-example>
       (define (make-adder x)
         (forthda x :+))

       (check-forth
        ()
        (7 (make-adder 5))
        (12))

       (struct :delay (v))

       (define/forth :apply
         #:lower stack
         (match-define (list* (:delay thing) more) stack)
         (forth #:stack more thing))

       (check-forth
        ()
        ((:delay (make-adder 5)) :dup 7 :swap :apply :swap :apply)
        (17))]

In other words, @racket[define/forth] is a trivial macro:

@chunk[<define/forth>
       (define-syntax-rule (define/forth name . body)
         (define name (forthda . body)))]

@section{Implementation: Definition Macro}

The @racket[forthda] macro uses some advanced macrology and is the
crown jewel of this implementation. Its basic structure is to expand
each @racket[forthda] into a stack transformer function, a new
sub-struct of @racket[forth-word], and an instance of that sub-struct
with the given stack transformer:

@chunk[<forthda-core>
       (define-syntax (forthda stx)
         (syntax-parse stx
           [(_ (~or (~seq ss:stack-spec
                          #:lift lifted:expr)
                    (~seq (~optional ss:stack-spec)
                          #:lower lstack:id lowered-body:expr ...)
                    (~seq (~optional ss:stack-spec)
                          normal-body:expr ...)))
            #:attr stack (or (attribute lstack) #'stack)
            (with-syntax
                ([body <forthda:body>]
                 [name-struct-defn <forthda:name-struct-defn>])
              (syntax/loc stx
                (let ()
                  (define (f stack) body)
                  name-struct-defn                  
                  (name-struct f))))]))]

Even at this level of abstraction, there's a few tricks. First, we use
@racket[~or] to combine the three different kinds of
definitions. Second, since the identifier used for the stack is only
required in the @racket[#:lower] case, we give it the default
@racket[#'stack] and use a different name for the combined
case. Third, notice that the stack effect specification is sometimes
required and sometimes optional. Finally, we rely on a definition of
the @racket[stack-spec] syntax class:

@chunk[<forthda-env>
       (begin-for-syntax
         (define-syntax-class stack-nat
           (pattern scount:nat 
                    <stack-nat-attrs>))

         (define-syntax-class stack-spec
           (pattern (input:stack-nat (~datum --) output:stack-nat)
                    <stack-spec-attrs>)))]

The next least complicated piece is the way we define the body of the
stack transformer, in the non-lifted case:

@chunk[<forthda:body>
       (cond
         [(attribute lifted)
          <forthda:body:lifted>]
         [(attribute lowered-body)
          (syntax/loc stx
            (begin lowered-body ...))]
         [else
          (syntax/loc stx
            (forth #:stack stack normal-body ...))])]

If we are lowering, then we just inject the body, otherwise we use the
given stack argument and hand everything over to the @racket[forth]
macro to do the association and evaluation.

The body of a lifted function is more complicated, because we need to
read a fixed number stack positions, call the Racket function, get
back a fixed number of results, then put them back on the stack. We
could use something like @racket[take] or @racket[split-at] for that,
but it would be expensive, since we know exactly how many positions
are needed at compile-time.

Given that we know those two numbers, we can generate the appropriate
number of temporary identifiers and then put them into two lists: one
going forward and one going backward. This enables the following
lifted body definition:

@chunk[<forthda:body:lifted>
       (syntax/loc stx
         (let ()
           (match-define (list* ss.in_n ... left-over) stack)
           (define-values (ss.out_0 ...) (lifted ss.in_0 ...))
           (list* ss.out_n ... left-over)))]

We read the input of the stack backwards, then call the function with
the input forwards, then read the results forwards, and put them on
the stack backwards. For the lifted definition of @racket[:+], this
looks like:

@chunk[<forthda:body:lifted:+>
       (syntax/loc stx
         (let ()
           (match-define (list* b a left-over) stack)
           (define-values (c) (+ a b))
           (list* c left-over)))]

We use the syntax classes @racket[stack-spec] and @racket[stack-nat]
to generate these lists of identifiers in their attributes:

@chunk[<stack-nat-attrs>
       #:attr [forward 1] 
       (generate-temporaries (build-list (syntax->datum #'scount) (λ (i) #'scount)))
       #:attr [backward 1] 
       (reverse (attribute forward))]

@chunk[<stack-spec-attrs>
       #:attr [in_0 1] (attribute input.forward)
       #:attr [in_n 1] (attribute input.backward)
       #:attr [out_0 1] (attribute output.forward)
       #:attr [out_n 1] (attribute output.backward)]

Beautiful, isn't it?

Finally, we have to define the struct for this @racket[forthda]. The
key here is that when the function has a stack specification, then
give the new struct the @racket[prop:procedure] structure property, so
Racket code will treat it as a function.

However, we have a dual problem to lifting: Racket will give us a
finite number of arguments, which we need to put into a stack and then
call the stack transformer, and read off a fixed number of answers.

And, of course, if there is no stack specification, we don't give it
the property, so Racket code can't call it:

@chunk[<forthda:name-struct-defn>
       (if (attribute ss)
         (syntax/loc stx
           (struct name-struct forth-word ()
                   #:property prop:procedure
                   (λ (so ss.in_0 ...)
                     (match-define
                      (list* ss.out_n ... left-over)
                      (f (list ss.in_n ...)))
                     (values ss.out_0 ...))))
         (syntax/loc stx
           (struct name-struct forth-word ())))]

At this point, we're done. The
@link["https://github.com/jeapostrophe/exp/blob/master/rorth/main.rkt"]{full
version of the system} is a tiny bit different and only has 85 lines
of code.

I haven't really used this for anything yet, but I think the macro is
beautiful and the @racket[:delay]/@racket[:apply] example is really
cool too.

If you'd like to run this code at home, you should put it in this
order:

@chunk[<*>
       (require rackunit
                racket/list
                racket/match
                (for-syntax syntax/parse
                            racket/base))

       (let () <ex1>)
       (let () <ex2/cc>)
       (let () <ex2/vs>)       

       <forth-data>
       <forth>
       <check-forth2>
       <forthda-env>
       <forthda-core>
       <define/forth>       

       (let ()
         <raw-dup>
         <dup-test>
         <dup-test2>)

       <forth-defns>
       
       <dup-test>
       <dup-test2>
       (let ()
         <check-forth1>
         <dup-test3>)
       <dup-test3>

       (let () <fex1>)
       (let () <fex2>)
       (let () <fex2/spec>)
       (let () <fex2/spec2>)

       (check-equal?
        (:sum-of-squares 1 2)
        5)
       (check-forth
        ()
        (1 2 :sum-of-squares)
        (5))

       <forthda-example>]

@(the-end)
