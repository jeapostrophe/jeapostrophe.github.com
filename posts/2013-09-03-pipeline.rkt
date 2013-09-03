#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Patterns: The Post-Process Pipeline}
@categories["Racket" "Macros" "Patterns"]

Many times in programs I find myself processing a large data structure
or complicated pattern and needing to have that processing inform the
rest of the program. In this post, I discuss a reoccurring pattern I
use in this situation.

@(the-jump)

Consider an implementation of the @code{wc} tool. This tool can count
characters, lines, or both. Here's a simple implementation of the core
in Racket:

@chunk[<wc-work>
       (define (wc-work show-chars? show-lines? f)
         (define-values (cs ls)
           (with-input-from-file f
             (λ ()
               (for/fold ([cs 0] [ls 0])
                   ([c (in-input-port-chars (current-input-port))])
                 (values (add1 cs)
                         (if (char=? c #\newline)
                           (add1 ls)
                           ls))))))
         (printf "~a:~a~a\n"
                 f
                 (if show-chars?
                   (format " ~ac" cs)
                   "")
                 (if show-lines?
                   (format " ~al" ls)
                   "")))]

We could then wrap this in a simple command line argument parser:

@chunk[<wc-imp>
       (define (wc-main args)
         (define show-chars? #t)
         (define show-lines? #t)
         (for ([a (in-list args)])
           (match a
             ["-l"
              (set! show-lines? #f)]
             ["-c"
              (set! show-chars? #f)]
             [f
              (wc-work show-chars? show-lines? f)])))]

I dislike this version because it uses mutation. It is fairly easy to
change it to not do that by integrating the mutated variables into the
@racket[for] loop with a @racket[for/fold]:

@chunk[<wc-fun>
       (define (wc-main args)
         (for/fold ([show-chars? #t]
                    [show-lines? #t])
             ([a (in-list args)])
           (match a
             ["-l"
              (values show-chars? #f)]
             ["-c"
              (values #f show-lines?)]
             [f
              (wc-work show-chars? show-lines? f)
              (values show-chars? show-lines?)]))
         (void))]

But I also dislike it because the work is integrated in to the parsing
of the arguments and it would be nicer to separate it.

@chunk[<wc-fun-sep>
       (define (wc-main args)
         (define-values
           (show-chars? show-lines? fs)
           (for/fold ([show-chars? #t]
                      [show-lines? #t]
                      [fs empty])
               ([a (in-list args)])
             (match a
               ["-l"
                (values show-chars? #f 
                        fs)]
               ["-c"
                (values #f show-lines?
                        fs)]
               [f
                (values show-chars? show-lines?
                        (cons f fs))])))
         (for ([f (in-list fs)])
           (wc-work show-chars? show-lines? f)))]

The next problem for me is that it relies heavily on each of the three
parsers for the different kinds of arguments need to know about all
the different options, so only one is changed while the others stay
the same. I think it is nicer to use a structure to remove the
@racket[values] to deal with this.

@chunk[<wc-fun-struct>
       (struct wc-config (show-chars? show-lines? fs))
       (define (wc-main args)
         (define c
           (for/fold ([c (wc-config #t #t empty)])
               ([a (in-list args)])
             (match a
               ["-l"
                (struct-copy wc-config c
                             [show-lines? #f])]
               ["-c"
                (struct-copy wc-config c
                             [show-chars? #f])]
               [f
                (struct-copy wc-config c
                             [fs (cons f (wc-config-fs c))])])))
         (for ([f (in-list (wc-config-fs c))])
           (wc-work (wc-config-show-chars? c) (wc-config-show-lines? c) f)))]

This is the style that I tend to prefer and it is particularly nice
for macros where each "argument" is a different kind of syntax
class. For instance, imagine @code{wc} as a macro:

@CHUNK[<wc-macro>
       (begin-for-syntax
         (struct wc-config (show-chars? show-lines? fs))
         (define-syntax-class arg
           #:attributes (transform)
           [pattern "-l"
                    #:attr transform
                    (λ (c)
                      (struct-copy wc-config c
                                   [show-lines? #f]))]
           [pattern "-c"
                    #:attr transform
                    (λ (c)
                      (struct-copy wc-config c
                                   [show-chars? #f]))]
           [pattern f:id
                    #:attr transform
                    (λ (c)
                      (struct-copy wc-config c
                                   [fs (cons #'f (wc-config-fs c))]))]))
       (define-syntax (wc-macro stx)
         (syntax-parse stx
           [(_ a:arg ...)
            (define c 
              (foldr (λ (f c) (f c))
                     (wc-config #t #t empty)
                     (attribute a.transform)))
            (quasisyntax/loc stx
              (for ([f (in-list (list #,@(wc-config-fs c)))])
                (wc-work #,(wc-config-show-chars? c) #,(wc-config-show-lines? c) f)))]))]

The macro case is where this pattern really shines. It is normally
very painful for a macro like @racket[wc-macro] to interface with each
of the various syntax classes that it uses inside of it. Since the
macro and the classes are typically both defined at the top-level, you
can't use the mutating approach. Since you don't control the parser,
you can't just add your own values to it. And if you don't create a
struct, then you have to have a smattering of arguments inside of your
fold, which gets very complicated.

I call this pattern the "post-process pipeline", because the parsers
return functions that use the result of parsing on the configurations,
so each kind of parser is given a uniform interface that just passes
the configuration down the pipeline.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

It is beautiful to separate parsing from the action of parsing and a
great way to do it is with an single value "transformer" function.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require racket/runtime-path
                racket/list
                racket/match
                (for-syntax racket/base
                            racket/list
                            syntax/parse))
       (define-runtime-path this
         "2013-09-03-pipeline.rkt")       

       <wc-work>

       (let ()
         <wc-imp>
         (wc-main (list this))
         (wc-main (list "-l" this))
         (wc-main (list "-c" this)))

       (let ()
         <wc-fun>
         (wc-main (list this))
         (wc-main (list "-l" this))
         (wc-main (list "-c" this)))

       (let ()
         <wc-fun-sep>
         (wc-main (list this))
         (wc-main (list "-l" this))
         (wc-main (list "-c" this)))

       (let ()
         <wc-fun-struct>
         (wc-main (list this))
         (wc-main (list "-l" this))
         (wc-main (list "-c" this)))

       <wc-macro>
       (wc-macro this)
       (wc-macro "-l" this)
       (wc-macro "-c" this)]

@(the-end)
