#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list
                     racket/generator
                     racket/file)
          "../post.rkt")

@title{The Evil Word Game Player}
@categories["Racket" "Games"]

Sometimes on long drives my wife and I play a game where we
collaboratively choose a word by alternating picking letters. The
loser of the game is whoever cannot choose a letter than maintains a
valid word, without completing a word either.

For instance, the sequence @litchar{yttrr} is not a prefix of any
word, but @litchar{yttr} is a prefix of @litchar{yttrium}, so if I
chose @litchar{Y}, my wife chose @litchar{t}, I chose @litchar{t}, my
wife chose @litchar{r}, and then I chose @litchar{r}, I would lose,
because I didn't maintain a valid word.

Similarly, I would lose if the sequence were @litchar{you}, because
that is a word and I would've made the last choice.

In this blog post, I present an algorithm for playing this game in the
most successful way.

@(the-jump)

The key to this game is that whoever goes first wants to choose a
letter with the smallest number of odd-lengthed words, because those
are the words that they lose with. An alternative way of looking at
this is that you want to choose a letter with the largest number of
even-lengthened words, because those are the words that the opponent
loses with. Using this simple idea, I create a trie of all prefix-free
words with a cache count of the number of even/odd-length valid
suffixes.

The data-structure just has a flag for if the current prefix is a
word, the counts, and then a suffix trie.

@chunk[<struct>
       (struct word-list (is-a-word? even-c odd-c suffix) #:mutable #:prefab)
       (define (make-empty-word-list)
         (word-list #f 0 0 (make-hasheq)))
       <add>]

The interesting work comes from adding a new word to the trie. I use
an interface with strings, but convert it to a character list to
process them one-by-one.

@chunk[<add>
       (define (word-list-add! wl str)
         (word-list-add-chars! wl (string->list str)))
       <addc>]

If the word list is a valid word, then I don't need to do anything
else, because the rules of the game mandate that any time a word is
given, the game ends, so I only need prefix-free words.

@chunk[<addc>       
       (define (word-list-add-chars! wl cs)
         (unless (word-list-is-a-word? wl)
           <add-not-word>))]

Otherwise there are two cases: if the word is over or not. If it is
over, then we set this word list to be a word and update its even
count. Otherwise we recur and update the counts.

@chunk[<add-not-word>
       (cond
         [(empty? cs)
          (set-word-list-is-a-word?! wl #t)
          (set-word-list-even-c! wl (add1 (word-list-even-c wl)))]
         [else
          (define n-wl
            (hash-ref! (word-list-suffix wl) (first cs) make-empty-word-list))
          (if (even? (length cs))
            (set-word-list-even-c! wl (add1 (word-list-even-c wl)))
            (set-word-list-odd-c!  wl (add1 (word-list-odd-c wl))))
          (word-list-add-chars! n-wl (rest cs))])]

Once the data-structure is around, I just use a standard source of
words, @filepath{/usr/share/dict/words}, and create a cached database
of words that are longer than two letters and contain only
letters. This database works out to be about two and a half megabytes.

@chunk[<cache>
       (define MIN 2)

       (define-runtime-path dict-raw "/usr/share/dict/words")
       (define-runtime-path dict-compiled "dict.rktd")
       
       (unless (file-exists? dict-compiled)
         (define *wl* (make-empty-word-list))

         (with-input-from-file dict-raw
           (λ ()
             (for ([l (in-lines)])
               (when (and ((string-length l) . > . MIN)
                          (regexp-match #rx"^[a-zA-Z]+$" l))
                 (word-list-add! *wl* (string-downcase l))))))

         (with-output-to-file dict-compiled
           #:exists 'replace
           (λ ()
             (write *wl*))))]

Although this whole program is less than a hundred lines, it is
particularly nice that the majority of that is data-structure. Even
better, the majority of the playing of the game is just providing the
user interface. It is slightly interesting to store the prefix
backwards.

@chunk[<play>
       (define (play wl w players-turn?)
         (cond
           [(or (not wl) (word-list-is-a-word? wl))
            (if players-turn?
              (printf "The computer lost.\n")
              (printf "The player lost.\n"))]
           [else
            (printf "The prefix is: ~a\n" (list->string (reverse w)))
            (define nc
              (cond
                [players-turn?
                 (printf "What's your letter? ")
                 (string-ref (read-line) 0)]
                [else
                 <computer-choice>
                 nc]))
            (play (hash-ref (word-list-suffix wl) nc #f)
                  (list* nc w)
                  (not players-turn?))]))]

The real interesting code is making the choice for the computer. We
just look through every valid next letter and pick the one with the
highest percentage of odd suffixes. (We want odd prefixes, because we
are choosing a letter, which means its odd suffixes are our even
suffixes.)

@chunk[<computer-choice>
       (define-values
         (nc n-wl %)
         (for/fold ([nc #f] [* #f] [% -inf.0])
             ([(c n-wl) (in-hash (word-list-suffix wl))])
           (define n-%
             (/ (word-list-odd-c n-wl)
                (+ (word-list-odd-c n-wl)
                   (word-list-even-c n-wl))))
           (if (n-% . > . %)
             (values c n-wl n-%)
             (values nc * %))))
       (printf "The computer choose ~a (~a ~a ~a).\n\t~e\n"
               (string nc)
               (word-list-odd-c n-wl)
               (word-list-even-c n-wl)
               %
               (word-list->string (list* nc w) n-wl))]

I find it frightening how painful it is to play against this
opponent. It selects very obscure words and is brutal. I'd like to
figure out a way to compress the information into a hand-sized card,
like the Tic-Tac-Toe solution card, so I can whomp people in
real-life.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

A good data-structure makes a simple algorithm.

Perfect computer players are not fun to play.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require racket/list
                racket/string
                racket/runtime-path)

       <struct>
       
       (define (word-list->list wl)
         (if (word-list-is-a-word? wl)
           (list empty)
           (append*
            (for/list ([(c n-wl) (in-hash (word-list-suffix wl))])
              (map (λ (ns) (cons c ns))
                   (word-list->list n-wl))))))
       (define (word-list->string prefix wl)
         (string-join
          (for/list ([w (in-list (word-list->list wl))])
            (apply string (append (reverse prefix) w)))))

       <cache>

       <play>

       (play (with-input-from-file dict-compiled read) empty
             (zero? (random 2)))]

Or just download the
@link["https://github.com/jeapostrophe/exp/blob/master/spell.rkt"]{raw
version}.

@(the-end)
