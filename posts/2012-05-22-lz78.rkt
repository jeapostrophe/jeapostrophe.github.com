#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "post.rkt")

@title{An LZ78 Implementation}
@categories["Compression" "Racket"]

For a long time I've known about and relied on the LZ77/78 compression
algorithms. Once when I was in middle school, I told a friend an idea
I had about compression, and the friend---who was in college at the
time---said it sounded exactly like LZ77. I don't remember what my
idea was, but I do remember that incident.

Despite this long connection, I'd never implemented the algorithm
before. I sought to rectify that situation.

@(the-jump)

First, I read about the algorithm on Wikipedia. The
@link["https://en.wikipedia.org/wiki/LZ77"]{article} is pretty
informative. I'll briefly recap it.

As you read through the content to compress, you keep track of a
dictionary of previously seen phrases. Whenever you discover a yet
undiscovered phrase, add it to the dictionary and encode it as the
previously-seen prefix and the new character. Decoding works in
reverse. Since the very first character of the encoded output is
necessarily not in the dictionary, its prefix will be empty and it
will establish the first phrase. As you read the encoding, you
maintain the same dictionary and decode by following the
phrase-reference backwards and then emitting the new character.

For an implementation, the signature of the function is pretty
straight-forward: it takes a character source, which I'll use an
input-port for, and it returns a list of pairs of the previously seen
reference and the new character.

However, there's one hitch. If the last phrase of the input is
previously seen, then there will be no right-hand side of the output
pair. You could add a special character to indicate that. I decided to
output just the phrase reference, in that case.

@section{Compression}

My compression code looks like this:

@chunk[<compress>
       (define (compress ip)
         <next-unseen>
         (let outer-loop ([next 1])
           (match (next-unseen next)
             [(? number? ref)
              (stream ref)]
             [(and W (cons ref c))
              (stream-cons W (outer-loop (add1 next)))])))]

The main work all happens in @racket[next-unseen] which takes the
reference that the next phrase will be given and either returns a
number, for the final reference in the stream, or a cons of the last
reference and the new character, which is added to the output and then
the compression continues. The dictionary itself is totally maintained
by the @racket[next-unseen] function.

@racket[next-unseen] runs in a loop keeping track of the current
dictionary and prefix phrase reference. At each iteration it reads a
byte from the input. There are then three cases:

1. The input is empty, in which case, the last seen phrase reference
is returned.

2. The current dictionary has a reference starting with that byte, in
which case, the prefix phrase is extended. This means the loop is
continued with a new dictionary and a new prefix phrase. For example,
if the current phrase is A, named 1, and the next input is B, and AB
is previously seen and named 2, then the dictionary will have a
mapping in it from B to a new dictionary and the number 2, which are
used in the next iteration of the loop.

3. The current dictionary does *not* have a reference for this byte,
meaning that we've encoded a new phrase. In that case, we can add this
byte to current dictionary and allocate a new name for it, then return
that new name.

Here's that in code:

@chunk[<next-unseen>
       (define top-dict (make-hasheq))
       (define (next-unseen this)
         (let loop ([dict top-dict]
                    [last 0])
           (define b (read-byte ip))
           (cond
             [(eof-object? b)
              last]
             [(hash-ref dict b #f)
              =>
              (λ (next)
                (loop (cdr next) (car next)))]
             [else
              (hash-set! dict b (cons this (make-hasheq)))
              (cons last b)])))]

This code uses a similar dictionary structure to my Boggle solver,
from the previous blog post. However, in this code, it's mutable
because the dictionary is extended as we go and it would be tedious to
thread the state.

I'm kind of amazed that the compression can fit in 23 lines!

Here's a little example:

@chunk[<compress-example>
       (define some-input #"AABABBBABAABABBBABBABB")
       (define compressed
         (compress
          (open-input-bytes some-input)))
       (define A (char->integer #\A))
       (define B (char->integer #\B))
       (check-equal?
        (stream->list
         compressed)
        (list (cons 0 A)
              (cons 1 B)
              (cons 2 B)
              (cons 0 B)
              (cons 2 A)
              (cons 5 B)
              (cons 4 B)
              (cons 3 A)
              7))]

In this example, the final dictionary looks like this:

@chunk[<compress-example-dict>
       (hasheq
        B 
        (cons 4 (hasheq B (cons 7 (hasheq))))
        A
        (cons 1
              (hasheq B
                      (cons 2
                            (hasheq B (cons 3 (hasheq A (cons 8 (hasheq))))
                                    A (cons 5 (hasheq B (cons 6 (hasheq)))))))))]

@section{Decompression}

Naturally, decompression is dual to compression. It will also maintain
a dictionary, but it will have the opposite information: rather than
mapping characters to references and suffixes, it will map references
to characters and prefixes.

The code is considerably simpler because there is a single
dictionary (rather than a structured one) and the decompression is a
fold over the input stream, rather than a more generative loop.

Here's the core of it:

@chunk[<decompress>
       (define (decompress str)
         (define dict (make-hasheq))
         <output-from-dict>
         (for/fold ([next 1])
             ([p (in-stream str)])
           (match p
             [(cons ref b)
              (hash-set! dict next p)
              (output-from-dict next)
              (add1 next)]
             [(? number? ref)
              (output-from-dict ref)
              next])))]

Basically, each element of the stream is either a new dictionary
entry, in which case we remember it and output it, or it's just a
reference and we output without remembering. Pretty simple. (We could
use a functional hash, but there's no benefit here.)

When you get a reference and need to output it, it's also quite easy:

@chunk[<output-from-dict>
       (define (output-from-dict this)
         (match (hash-ref dict this #f)
           [#f
            (void)]
           [(cons last this-b)
            (output-from-dict last)
            (write-byte this-b)]))]

Either the reference isn't in the dictionary, so you stop, or it is,
so you output its prefix and then the byte associated with it. We use
the stack as our data-structure to keep track of bytes to write,
because the dictionary stores the prefixes, not the suffixes.

The whole decompression is just 20 lines. Wow!

We can check that the output is the same as the input:

@chunk[<decompress-example>
       (check-equal?
        (with-output-to-bytes
         (λ ()
           (decompress compressed)))
        some-input)]

In the example, the dictionary is:

@chunk[<decompress-example-dict>
       (hasheq 8 (cons 3 A)
               7 (cons 4 B)
               6 (cons 5 B)
               5 (cons 2 A)
               4 (cons 0 B)
               3 (cons 2 B)
               2 (cons 1 B)
               1 (cons 0 A))]

And that's it!

@section{Further work}

One strange thing about this implementation is that the output is just
a stream of pairs rather than bytes. The easiest way to encode it as
bytes is to write each pair as two bytes. That's not totally correct,
however, because the number of prefixes may exceed the number of
bytes, so you'll need to use the length of the input log-2 for the
number of bits per reference. You can do a little bit better by having
the decoder keep track of this number during decoding and gradually
use more bits.

It is interesting to think of what kinds of input this algorithm fails
to compress. It's those without common prefixes. For example, if you
had all the bytes from 0 to 255, then the output would be double (plus
one! Why?) the input. If you then added each byte to every other byte,
then you'd double again.

Another surprising thing about the algorithm is that the dictionary is
just as long as the compressed output because the compressed output IS
the dictionary. The compression algorithm is inherently non-random
access because you need to read it linearly to know the context of the
prefix references.

This was a very fun thing to implement. I hope you enjoy it!

By the way, if you use this code at home, make sure you put the code
in this order:

@chunk[<*>
       (require rackunit
                racket/list
                racket/match
                racket/stream
                racket/port)

       <compress>
       <compress-example>
       <compress-example-dict>
       <decompress>
       <decompress-example>
       <decompress-example-dict>
       ]
