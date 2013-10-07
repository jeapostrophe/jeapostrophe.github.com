#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list
                     racket/generator
                     racket/file)
          "../post.rkt")

@title{Saving State with Dynamic Wind}
@categories["Racket" "Continuations"]

@racket[dynamic-wind] is a function that allows you to write code that
is "robust" in the face of time travel through continuations. We wrote
about it @postlink["2012-07-16-cont-mar"]{once before} but there's
more to be said.

@(the-jump)

@racket[dynamic-wind] works by taking three functions that I call
@racket[before-f], @racket[body], and
@racket[after-f]. @racket[dynamic-wind] guarantees that whenever
@racket[body] is called, @racket[before-f] was just called and whenever
@racket[body] exits @racket[after-f] will be called. Finally, the value
returned is the same @racket[body].

In a programming language without exceptions, you could implement it
via:

@chunk[<fake-dw1>
       (define (dynamic-wind before-f body after-f)
         (begin (before-f)
                (begin0 (body)
                        (after-f))))]

But because exceptions may occur inside of @racket[body] and cause
control to jump past @racket[after-f], it has to be written more like:

@chunk[<fake-dw2>
       (define (dynamic-wind before-f body after-f)
         (define raised? #f)
         (define raise-v #f)
         (begin (before-f)
                (begin0 (with-handlers ([exn? 
                                         (λ (x) 
                                           (set! raised? #t)
                                           (set! raise-v x))])
                          (body))                        
                        (after-f)
                        (when raised? (raise raise-v)))))]

But things are even more complicated with escape continuations and
continuation aborts, because control could leave @racket[body] without
throwing an exception. Furthermore, @racket[body] could capture a
continuation and give to the outside. If it were ever called, then we
would go back inside @racket[body] and thus need to re-run
@racket[before-f]. This is why @racket[dynamic-wind] needs to be a
primitive of Racket.

So, given this primer, what's the point of @racket[dynamic-wind]? It
is normally described as being used to ensure that @racket[body] sees
a consistent view of state despite time travel. Let's see how true
that is.

First, we'll need a temporary file with some values in it:

@chunk[<make-file>
       (define f (make-temporary-file))
       (with-output-to-file f 
         #:exists 'replace
         (λ ()
           (for ([i (in-range 5)])
             (write (list i)))))

       (check-equal?
        (file->list f)
        '((0) (1) (2) (3) (4)))]

Now, let's write a function that allows another function to reliably
read from this file. We'll call the function
@racket[call-with-input-file*]. It's job is to open a file, give it to
a function, and close it if the function exits. We to ensure that the
function's use of the file is robust in the face of time
travel. Here's a simple use:

@chunk[<for-use>
       (check-equal?
        (call-with-input-file* f
          (λ (ip)
            (for/list ([v (in-port read ip)])
              v)))
        '((0) (1) (2) (3) (4)))]

If we want to expand away the use of @racket[for/list] and
@racket[in-port], this is the same as:

@chunk[<loop-use>
       (check-equal?
        (call-with-input-file* f
          (λ (ip)
            (let loop ()
              (define v (read ip))
              (if (eof-object? v)
                empty
                (cons v (loop))))))
        '((0) (1) (2) (3) (4)))]

The most obvious way to write this function is as follows:

@chunk[<cwif1>       
       (define (call-with-input-file* p f)
         (define ip #f)
         (dynamic-wind
             (λ () 
               (set! ip (open-input-file p)))
             (λ ()
               (f ip))
             (λ ()
               (close-input-port ip)
               (set! ip #f))))]

Let's see how well this version handles time travel! We'll use
@racketmodname[racket/generator] to simulate time travel. This module
provides generators that can gradually return streams of
values. Whenever a value is returned (with @racket[yield]), we go back
to the context of the caller (an exit) and whenever a value is
requested we go back to the context of the generator (a jump).

Our generator will gradually read each one of the values in the file
and @racket[yield] them until it reaches the end, which it will signal
with @racket[#f]. We'll construct a list of all the things
@racket[yield]ed. However, notice that we also iterate through the
naturals up to @racket[6]. Remember that there are only @racket[5]
values in the file though, so the @racket[in-producer] should end
first!

@chunk[<generator>
       (define f-generator
         (generator ()
                    (call-with-input-file* f
                      (λ (ip)
                        (let loop ()
                          (define v (read ip))
                          (if (eof-object? v)
                            (yield #f)
                            (begin (yield v)
                                   (loop))))))))
       (for/list ([v (in-producer f-generator #f)]
                  [i (in-range 6)])
         v)]

We'll put this in a macro to try it out with different implementations
of @racket[call-with-input-file*].

@chunk[<try>
       (define-syntax-rule (try-generator call-with-input-file*)
         (let ()
           <generator>))]

If we try this with our first attempt, then it fails with the error
@litchar{input port is closed} because the continuation closes over
the particular port it was given the first time, which is now closed.

@chunk[<attempt1>
       (check-exn
        (λ (x) (regexp-match #rx"input port is closed" (exn-message x)))
        (λ () (try-generator call-with-input-file*)))]

We could fix this two ways. The best way would be to change
@racket[call-with-input-file*] so it gave a @racket[box] with an input
port inside, to expose that the port can change. However we'd have to
rewrite @racket[<generator>] to do that, so let's just turn
@racket[call-with-input-file*] into a macro that knows the structure
of its argument and can directly modify the argument:

@chunk[<cwif2>
       (define-syntax-rule (call-with-input-file*/macro p (λ (ip) body ...))
         (let ()
           (define ip #f)
           (dynamic-wind
               (λ () 
                 (set! ip (open-input-file p)))
               (λ ()
                 body ...)
               (λ ()
                 (close-input-port ip)
                 (set! ip #f)))))]

This will ensure that the body of the generator will get the correct
input port every time it gets control. Unfortunately when we run the
test, we find that it returns a list of @racket[6] @racket[0] lists!
This is one too many and it's always the first value!

@chunk[<attempt2>
       (check-equal?
        (try-generator call-with-input-file*/macro)
        '((0) (0) (0) (0) (0) (0)))]

If we didn't include the @racket[in-range] in our test, then it would
have been an infinite loop!

The problem is that our use of @racket[dynamic-wind] doesn't restore
all the state. In particular, it doesn't restore the position within
the file that we were reading! We can fix that, of course, using
@racket[file-position]:

@chunk[<cwif3>
       (define-syntax-rule (call-with-input-file*/macro+fseek p (λ (ip) body ...))
         (let ()
           (define ip #f)
           (define pos 0)
           (dynamic-wind
               (λ () 
                 (set! ip (open-input-file p))
                 (file-position ip pos))
               (λ ()
                 body ...)
               (λ ()
                 (set! pos (file-position ip))
                 (close-input-port ip)
                 (set! ip #f)))))]

After this change, our generator example returns the appropriate
number of values and all the right ones too!

@chunk[<attempt3>
       (check-equal?
        (try-generator call-with-input-file*/macro+fseek)
        '((0) (1) (2) (3) (4)))]

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

@racket[dynamic-wind] is necessary in a world of continuations to
write code robust against time travel.

Sometimes being robust against time travel means exposing that time
travel is possible and things can change from beneath the code using
mutation.

It is not always obvious what state needs to be preserved across
continuation jumps.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require racket/list
                racket/file
                rackunit
                racket/generator)
       
       (let ()
         (let () <fake-dw1> (void))
         (let () <fake-dw2> (void)))

       <make-file>

       <cwif1>
       <for-use>
       <loop-use>
       <try>
       <attempt1>

       <cwif2>
       <attempt2>

       <cwif3>
       <attempt3>
       
       (delete-file f)]

@(the-end)
