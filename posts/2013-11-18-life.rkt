#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list
                     racket/generator
                     racket/file)
          "../post.rkt")

@title{Conway's Game of Life in Racket}
@categories["Racket"]

I find
@link["http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life"]{Conway's
Game of Life} to be a fascinating idea. I decided to try to make a
very short and efficient implementation today in Racket.

@(the-jump)

The Game of Life involves an infinite two-dimensional grid where each
grid cell just contains a boolean that is interpreted as whether the
cell is "alive". There are very simple rules to determine how one grid
configuration changes into the next configuration: a cell is dead
unless in the previous state it has three living neighbors or was
alive and had two living neighbors. A particular game is interesting
if has an interesting seed. For instance, this is the Gosper glider
gun:

@chunk[<example>
       (let-there-be-life
        (string-append
         "........................O...........\n"
         "......................O.O...........\n"
         "............OO......OO............OO\n"
         "...........O...O....OO............OO\n"
         "OO........O.....O...OO..............\n"
         "OO........O...O.OO....O.O...........\n"
         "..........O.....O.......O...........\n"
         "...........O...O....................\n"
         "............OO......................"))]

The key to implementing the Game of Life is an efficient
representation of the grid, which I call the "dish" vector. I chose to
use a byte array stored in column major order:

@chunk[<dishv>
       (define (make-dishv rs cs)
         (make-bytes (* rs cs)))
       (define-inline (dishv-set! dv rs i j ?)
         (unsafe-bytes-set! 
          dv (unsafe-fx+ i (unsafe-fx* rs j))
          (if ? 1 0)))
       (define-inline (dishv-ref dv rs i j)
         (unsafe-fx= 
          1 
          (unsafe-bytes-ref 
           dv (unsafe-fx+ i (unsafe-fx* rs j)))))]

Once I have this framework, I represent the entire dish as two dish
vectors plus the size of the grid. The reason to have two vectors is
to limit allocation by constantly using the same two arrays, but you
need at least two because you read one as you write the other. The
parser for the string representation is pretty boring:

@chunk[<parse>
       (struct dish (rows cols cur nxt) #:mutable)
       (define (string->dish s)
         (define rows (string-split s))
         (define rs
           (* 2 (length rows)))
         (define cs
           (* 1 (apply max (map string-length rows))))
         (define cur (make-dishv rs cs))
         (define nxt (make-dishv rs cs))

         (for ([i (in-naturals)]
               [r (in-list rows)])
           (for ([j (in-naturals)]
                 [c (in-string r)])
             (dishv-set! cur rs i j (char=? #\O c))))

         (dish rs cs cur nxt))]

The next interesting thing is a way to figure out how many living
neighbors a particular cell has. I made a highly optimized version, so
it's pretty ugly, but the basic premise is to sum up all the
neighbors, while avoiding going off the screen or considering the cell
itself:

@chunk[<neighbors>       
       (define-syntax-rule (unsafe-between min x max)
         (and (unsafe-fx<= min x)
              (unsafe-fx< x max)))
       (define-inline (neighbors gv rs cs i j)
         (let ([cnt 0])
           (for ([di (in-range -1 +2)])
             (let ([ni (unsafe-fx+ di i)])
               (when (unsafe-between 0 ni rs)
                 (for ([dj (in-range -1 +2)])
                   (unless (and (unsafe-fx= di 0) (unsafe-fx= dj 0))
                     (let ([nj (unsafe-fx+ dj j)])
                       (when (and (unsafe-between 0 nj cs)
                                  (dishv-ref gv rs ni nj))
                         (set! cnt (unsafe-fx+ 1 cnt)))))))))
           cnt))]

After you can figure out how many neighbors there are, the actual
transition is dirt simple. The only interesting thing is that I use
the same structure to ensure that I don't do any allocation.

@chunk[<tick>       
       (define (tick d)
         (match-define (dish rs cs cur nxt) d)
         (for* ([i (in-range rs)]
                [j (in-range cs)])
           (define alive? (dishv-ref cur rs i j))
           (define ns (neighbors cur rs cs i j))
           (define new-alive?
             (or (and alive? (or (unsafe-fx= ns 2) (unsafe-fx= ns 3)))
                 (and (not alive?) (unsafe-fx= ns 3))))
           (dishv-set! nxt rs i j new-alive?))
         (set-dish-cur! d nxt)
         (set-dish-nxt! d cur)
         d)]

This code is a bit ugly because of all the "unsafe" stuff, but it is
very fast, especially compared to the original version. I used this
code to benchmark the Gosper gun to ten thousand generations:

@chunk[<benchmark>
       (define (let-there-be-life s)
         (define seed (string->dish s))
         (collect-garbage)
         (collect-garbage)
         (time
          (for ([i (in-range 10000)])
            (tick seed))))]

The original version was 1800 milliseconds. Once I optimized the dish
vector code, I shaved off 200 milliseconds. The real speed came from
optimizing @racket[neighbors], which dropped it down to 500
milliseconds.

However, the Game is mainly exciting when you get to watch it. So I
wrote a small visualizer too:

@chunk[<view>       
       (define (draw d)
         (match-define (dish rs cs cur _) d)
         (define SCALE 10)
         (define BOX
           (square SCALE "solid" "black"))
         (for*/fold ([img (empty-scene
                           (* SCALE cs)
                           (* SCALE rs))])
             ([i (in-range rs)]
              [j (in-range cs)])
           (if (dishv-ref cur rs i j)
             (place-image BOX 
                          (+ (/ SCALE 2) 0.5 (* j SCALE))
                          (+ (/ SCALE 2) 0.5 (* i SCALE))
                          img)
             img)))

       (define (let-there-be-life s)
         (big-bang (string->dish s)
                   [on-tick tick]
                   [on-draw draw]))]

It was actually pretty important for the visualizer to be done this
way, so that only live cells caused allocation and new images to be
constructed. I originally wrote it as a fairly naive version were
every cell contributed, but that ran incredibly slowly.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

The Game of Life is cool and can be implemented in less than a hundred
lines of efficient Racket code.

Through judicious use of unsafe operations and inlining, you can
drastically improve your Racket code's performance.

If you'd like to run this exact code at home, you should put it in
this order:

@CHUNK[<*>
       (require racket/string
                racket/match
                racket/unsafe/ops
                racket/performance-hint
                2htdp/universe
                2htdp/image)

       <dishv>
       <parse>
       <neighbors>
       <tick>

       (let ()
         <benchmark>
         <example>)

       (let ()
         <view>
         <example>)]

Or just download the
@link["https://github.com/jeapostrophe/exp/blob/master/life.rkt"]{raw
version}.

@(the-end)
