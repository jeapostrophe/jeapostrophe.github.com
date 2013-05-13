#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Writing a Vi-like Graphics Editor in Racket}
@categories["Racket" "GUI"]

If you know me well, you know I hate mice and love
keyboards. Naturally, this means I love things like
@link["http://en.wikipedia.org/wiki/Vi"]{vi},
@link["http://en.wikipedia.org/wiki/Emacs"]{Emacs},
@link["http://en.wikipedia.org/wiki/Conkeror"]{Conkeror}, and
@link["http://en.wikipedia.org/wiki/Xmonad"]{xmonad}.

Unfortunately, there are not a lot of keyboard-based graphics editors,
and I am in the need of some help creating sprite graphics for my
@link["https://github.com/get-bonus/get-bonus"]{Get Bonus}
project. So, I wrote my own editor in Racket:
@link["https://github.com/get-bonus/get-bonus/tree/master/tools/apse"]{apse}
- the Animated Paletted Sprite Editor.

There are a few things interesting about it, but in this post, I focus
on the core of its job: dropping pixels and changing colors. The best
part is how the minibuffer works.

@(the-jump)

@section{Framework}

The first thing we need to do is to create a window (called a
@emph{frame}) that contains a canvas and a status line.

@chunk[<create1>
       (define mw (new frame% [label "vishop"]))
       (define buffer (new canvas% [parent mw]))
       (send mw create-status-line)
       (send mw show #t)]

However, if we want to be able to receive input events, we'll need to
customize the @racket[on-char] handler of the canvas and give it
focus. We have to create a sub-class of @racket[canvas%] to do that:

@chunk[<create2>
       (define mw (new frame% [label "vishop"]))

       (define our-canvas%
         (class canvas%
           (define/override (on-char ke)
             (eprintf "got ~v\n" (send ke get-key-code)))
           (super-new)))

       (define buffer (new our-canvas% [parent mw]))
       (send buffer focus)

       (send mw create-status-line)
       (send mw show #t)]

Next, we should customize the canvas further so that we are in control
of how it draws. This is done through an method called
@racket[on-paint] which is responsible for doing the drawing. We need
to make sure to regularly refresh the canvas to cause it to redraw and
the key event handler is a convenient place to do that.

@chunk[<create3>
       (define mw (new frame% [label "vishop"]))

       (define our-canvas%
         (class canvas%
           (inherit refresh-now get-dc get-width get-height)

           (define/override (on-char ke)
             (eprintf "got ~v\n" (send ke get-key-code))
             (refresh-now))

           (define/override (on-paint)
             (define dc (get-dc))
             (send dc set-pen "black" 0 'solid)
             (send dc set-brush "blue" 'solid)
             (send dc draw-ellipse
                   (/ (get-width) 2)
                   (/ (get-height) 2)
                   (/ (get-width) 4)
                   (/ (get-height) 4)))

           (super-new)))

       (define buffer (new our-canvas% [parent mw]))
       (send mw create-status-line)
       (send mw show #t)
       (send buffer focus)]

This is the basic framework of everything we'll add.

@section{Version One: Movement and a Bitmap}

The next step will be to change so that we are editing a bitmap and
displaying it.

The first step is to represent the bitmap, where the cursor is, and
what the current drawing color is. For convenience, we'll make the
bitmap always a nice even 64. (Very large for a sprite, actually.)

@chunk[<v1-state>
       (define w 64)
       (define h 64)
       (define the-bm (make-object bitmap% w h #f #t))
       (define the-bm-dc (send the-bm make-dc))
       (define x 0)
       (define y 0)
       (define current-c (make-object color% 0 0 0 1.0))]

Next, we'll need to customize the @racket[on-char] handler so you can
use the arrow keys to move around, use the space bar to drop a pixel,
and press @litchar{q} to quit.

@chunk[<v1-on-char>
       (define/override (on-char ke)
         (match (send ke get-key-code)
           [#\space
            (send the-bm-dc set-pixel x y current-c)]
           ['left
            (set! x (modulo (sub1 x) w))]
           ['right
            (set! x (modulo (add1 x) w))]
           ['up
            (set! y (modulo (sub1 y) h))]
           ['down
            (set! y (modulo (add1 y) h))]
           [#\q
            (exit 0)]
           [x
            (eprintf "got ~v\n" x)])
         (refresh-now))]

Finally, the @racket[on-paint] method must change to actually draw the
bitmap on the canvas. We'll draw it scaled (with an integer scale) and
in the center of the canvas. This will help it maintain crispness,
while keeping it easy to see.

@chunk[<v1-on-paint>
       (define/override (on-paint)         
         (define dc (get-dc))
         (define it (send dc get-transformation))
         (send dc set-smoothing 'unsmoothed)

         (define cw (get-width))
         (define ch (get-height))

         (define the-scale
           (floor (min (/ cw w) (/ ch h))))
         (send dc translate
               (/ (- cw (* w the-scale)) 2)
               (/ (- ch (* h the-scale)) 2))

         (send dc set-scale the-scale the-scale)

         (send dc draw-bitmap the-bm 0 0)

         (send dc set-transformation it))]

There are a few cute things about this drawing routine: It saves the
transformation matrix to return the state back to the beginning, so we
don't repeatedly zoom in. It uses the @racket['unsmoothed] mode to get
deliciously jagged pixels. It gets the canvas dimensions every draw to
deal with window resizing.

This all gets inserted into our framework:

@chunk[<v1-create>
       (define mw (new frame% [label "vishop"]))

       (define our-canvas%
         (class canvas%
           (inherit refresh-now get-dc get-width get-height)
           <v1-state>
           <v1-on-char>
           <v1-on-paint>
           (super-new)))

       (define buffer (new our-canvas% [parent mw]))
       (send mw create-status-line)
       (send mw show #t)
       (send buffer focus)]

As an exercise, you should add something to display an outline around
where the cursor is. You'll want to use @racket[draw-rectangle].

@section{Version Two: Using the Status Line}

Let's use the status line to communicate with the user about simple
things, like where the cursor is and what color they just wrote. For
fun, we'll add how long the command took to execute. We just need to
customize the @racket[on-char] handler for that: we'll have the
key-code match return a string which will be the new status text.

@chunk[<v2-on-char>      
       (define/override (on-char ke)         
         (define start (current-inexact-milliseconds))
         (define new-status
           (match (send ke get-key-code)
             [#\space
              (send the-bm-dc set-pixel x y current-c)
              (format "drew ~a at ~a,~a" (color%->string current-c) x y)]
             ['left
              (set! x (modulo (sub1 x) w))
              (format "left to ~a,~a" x y)]
             ['right
              (set! x (modulo (add1 x) w))
              (format "right to ~a,~a" x y)]
             ['up
              (set! y (modulo (sub1 y) h))
              (format "up to ~a,~a" x y)]
             ['down
              (set! y (modulo (add1 y) h))
              (format "down to ~a,~a" x y)]
             [#\q
              (exit 0)]
             [x
              (eprintf "got ~v\n" x)
              #f]))
         (define end (current-inexact-milliseconds))
         (when new-status
           (send mw set-status-text
                 (format "~ams ~a"
                         (real->decimal-string (- end start))
                         new-status)))
         (refresh-now))]

It fits in the framework just as before:

@chunk[<v2-create>
       (define mw (new frame% [label "vishop"]))

       (define our-canvas%
         (class canvas%
           (inherit refresh-now get-dc get-width get-height)
           <v1-state>
           <v2-on-char>
           <v1-on-paint>
           (super-new)))

       (define buffer (new our-canvas% [parent mw]))
       (send mw create-status-line)
       (send mw show #t)
       (send buffer focus)]

@section{Version Three: Implementing the Mini-Buffer}

The only remaining things we'll want to add to the editor is a way to
save the image and a way to change the color. Unlike our previous
commands, these both require more important from the user: the file
name and the new color. One obnoxious way to handle these would be
with a pop-up textbox, but our goal is to implement something like how
vi/emacs/etc work, where the user types at the "minibuffer"---which is
like our status line.

It would seem that we must add some sort of global state to our
program that recognizes when we are attempting to communicate with the
user, and if so, handle keys differently, and then after it's done
remember why we were trying to interact and handle it
appropriately. The code would look something like this:

@chunk[<v3-on-char:strawman>
       (define doing-something-else #f)
       (define/public (on-char ke)
         (match doing-something-else
           ['saving
            ....
            record key presses
            (when hit-enter?
              do the save)
            ....]             
           [#f
            ....
            [#\s
             (set! doing-something-else 'saving)]
            ....]))]

Obviously, I wouldn't be writing this if we were really going to do
something so ugly. Instead, we'll write code like this---focusing on
the first two cases:

@chunk[<v3-on-char>      
       (define/override (on-char ke)         
         (define start (current-inexact-milliseconds))
         (define new-status
           (with-minibuffer ke
            (match (send ke get-key-code)
              [#\s
               (define name (minibuffer-read "Filename"))
               (send the-bm save-file (format "~a.png" name) 'png 100)
               (format "saved to ~a.png" name)]
              [#\c
               (define r (string->number (minibuffer-read "Red")))
               (define g (string->number (minibuffer-read "Green")))
               (define b (string->number (minibuffer-read "Blue")))
               (set! current-c (make-object color% r g b 1.0))
               (format "set color to ~a" (color%->string current-c))]
              [#\space
               (send the-bm-dc set-pixel x y current-c)
               (format "drew ~a at ~a,~a" (color%->string current-c) x y)]
              ['left
               (set! x (modulo (sub1 x) w))
               (format "left to ~a,~a" x y)]
              ['right
               (set! x (modulo (add1 x) w))
               (format "right to ~a,~a" x y)]
              ['up
               (set! y (modulo (sub1 y) h))
               (format "up to ~a,~a" x y)]
              ['down
               (set! y (modulo (add1 y) h))
               (format "down to ~a,~a" x y)]
              [#\q
               (exit 0)]
              [x
               (eprintf "got ~v\n" x)
               #f])))
         (define end (current-inexact-milliseconds))
         (when new-status
           (send mw set-status-text
                 (format "~ams ~a"
                         (real->decimal-string (- end start))
                         new-status)))
         (refresh-now))]

The key is the @racket[with-minibuffer] form that allows the use of
the minibuffer and the @racket[minibuffer-read] function which prompts
the user for input.

The main idea of these functions is that @racket[with-minibuffer] sets
up a continuation prompt and gives control to the minibuffer code if
there is a @racket[minibuffer-read] call active.

@chunk[<with-minibuffer>
       (define minibuffer-run! #f)
       (define minibuffer-prompt
         (make-continuation-prompt-tag 'minibuffer))
       (define-syntax-rule (with-minibuffer ke e)
         (call-with-continuation-prompt
          (λ ()
            (if minibuffer-run!
              (minibuffer-run! ke)
              e))
          minibuffer-prompt))]

It is the responsibility of @racket[minibuffer-read] to capture the
continuation back to the prompt, then escape back to the prompt with
the initial prompt. When the @racket[return-to-minibuffer-call]
continuation is called, it uninstalls itself and returns the value
from the read interaction.

@chunk[<minibuffer-read>
       (define (minibuffer-read prompt)
         (begin0
           (call/cc 
            (λ (return-to-minibuffer-call)
              (define input-so-far "")
              (set! minibuffer-run! <minibuffer-run-body>)
              (abort-current-continuation 
               minibuffer-prompt
               (λ () (format "~a > " prompt))))
            minibuffer-prompt)
           (set! minibuffer-run! #f)))]

The body of the minibuffer handler is fairly routine: It is in the
context of @racket[input-so-far], which a string it uses to track what
the user has typed. It looks at the key event and implements
@litchar{return} as a signal to return the value, @litchar{backspace}
as removing the last character, and otherwise accumulates
characters. The only interesting part is the way it handles the escape
key as canceling the interaction, so it uses a pun on the use of
@racket[input-so-far] to give a cancellation message.

@chunk[<minibuffer-run-body>
       (λ (ke)                              
         (match (send ke get-key-code)
           [#\return
            (return-to-minibuffer-call input-so-far)]
           [#\backspace
            (unless (string=? "" input-so-far)
              (set! input-so-far
                    (substring
                     input-so-far 0
                     (sub1
                      (string-length input-so-far)))))]
           [(and (? char? c) 
                 (or (? char-alphabetic?)
                     (? char-numeric?)))
            (set! input-so-far
                  (string-append input-so-far
                                 (string c)))]
           ['escape
            (set! minibuffer-run! #f)
            (set! input-so-far "[canceled]")]
           [_ (void)])
         (format "~a > ~a" prompt input-so-far))]

When we plug this in to our framework, we have a basic key-oriented
image editor.

@chunk[<v3-create>
       (define mw (new frame% [label "vishop"]))

       (define our-canvas%
         (class canvas%
           (inherit refresh-now get-dc get-width get-height)
           <v1-state>
           <with-minibuffer>
           <minibuffer-read>
           <v3-on-char>
           <v1-on-paint>
           (super-new)))

       (define buffer (new our-canvas% [parent mw]))
       (send mw create-status-line)
       (send mw show #t)
       (send buffer focus)]

@section{Full Version}

The full version of the
@link["https://github.com/get-bonus/get-bonus/blob/master/tools/apse/mb-frame.rkt"]{minibuffer
code} (only 176 lines) adds a lot more: tab completion using a prefix
trie, controlling valid characters and accept predicates, etc.

The full version of the
@link["https://github.com/get-bonus/get-bonus/blob/master/tools/apse/main.rkt"]{image
editor} (only 593 lines) adds even more: palettes, view the image at
different resolutions, constructing animations, etc.

I made the only system fairly modular so I could re-use a lot of code
and create a
@link["https://github.com/get-bonus/get-bonus/blob/master/tools/apse/sprite-cut.rkt"]{sprite
sheet cutting tool} at a very low cost: only 315 lines.

If you'd like to run this code at home, you should put it in this
order:

@chunk[<*>
       (require racket/gui/base
                racket/class
                racket/match)

       (define (color%->string c)
         (format "(~a,~a,~a,~a)"
                 (send c red)
                 (send c green)
                 (send c blue)
                 (send c alpha)))

       (when #f <create1>)
       (when #f <create2>)
       ;; (when #f <create3>)
       ;; (when #f <v1-create>)
       ;; (when #f <v2-create>)
       <v3-create>]

@(the-end)
