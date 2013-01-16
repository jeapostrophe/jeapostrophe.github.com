#lang scribble/manual
@(require "../post.rkt"
(for-label racket/base
                     rackunit
                     racket/list))

@title{The Optimal Shower}
@categories["Efficiency"]

One of my goals in life is to do things very efficiently and get a lot
done. But throughout my whole five-year marriage, my wife has always
complained about my long showers, which to her were a waste of time
and valuable water.

As a computer scientist and programmer, I try to use technology to
optimize my life whenever possible. In this article, I discuss how I
improved my shower times and my marriage.

@(the-jump)

Before I start, two pieces of background information.

First, in the first two weeks or so of knowing my wife, she happened to see
me brush my teeth. I left on the water while I brushed and then rinsed
off with the same stream of water. She was horrified. She told me how
it was bad to do that and referenced a Public Service Announcement
from her childhood talking about that which called it out as "wasting
Utah." (Utah is a desert with very little rain: most places where
people live get about 15 inches per year.) Based on this, I repented
and have never left on the water since. (Marriage advice: Do whatever
your wife says.)

Second, I've heard of people who take EXTREMELY long showers, like 30
minutes to an hour. I was more in the 15 minute to 30 minute boat,
probably averaging around 20 minutes.

Once we lived together, my wonderful wife started making little
comments about how long my showers were. I made a few attempts to
speed them up. The first was to play a set of a few albums during the
shower so I'd know how long it was taking. Another was to always do
the same thing so I would have a regular order of operations and thus
go a bit faster. These didn't really make a significant difference. I
was probably consistently at 20 minutes and never more.

Next, I decided that I would "time" myself, in a sense, while in the
shower. I would just count (out loud or in my head) as I was in
there. This cut off about 5 minutes on average, probably, down to
about 15 minutes.

Next, I went a bit further. I would count out each
shower "activity"---such as washing my left arm, etc. I'd give myself
to the count of five. (But when I counted to five, it probably really
lasted between 10 and 15 seconds.) This was very effective and dropped
me down to about 10 minutes per shower.

But I knew I could do better. I just needed some technological help to
keep me focused.

Here's the concept:

* Plan out every activity that you're going to do. I came up with 22
things on days I washed my hair and 15 for other days.

* Give yourself 10 seconds per activity.

* Have a music file that "announces" what activity you should be doing
at each time.

* Make it exciting by playing intense music (different music for each
activity) during the activity.

* End with a horrible alarm so you will get /out/ of the shower
immediately.

With this concept in mind, I created two MP3s:

* @link["/downloads/code/tmp/2012-08-06-shower.mp3"]{The Shower Song}

* @link["/downloads/code/tmp/2012-08-06-shower-hair.mp3"]{The Shower Song (w/ hair)}

As my background music, I used the soundtrack for Mega Man 2 for the
NES, by Capcom. For the announcer, I used the espeak system. For the
alarm, I found a horrible alarm on freesound.org.

With this in place, my showers are now either 2:43 (including alarm)
or 3:51 (including alarm), depending on whether I am washing my hair
on that day. Normally I have about 10 seconds of alarm while I'm
drying off, though.

Whereas before, a shower would keep me comfortable and lull me back to
sleep a little, now showers are a frenetic rush as I try to get
everything done in about 10 seconds. It is exhilarating and extremely
satisfying to have so much more free time as part of my morning.

You can see the program I used to generate the audio here:

@link["https://github.com/jeapostrophe/exp/blob/master/shower.rkt"]{shower.rkt}

I highly recommend trying this out.

You may be wondering what my wife thinks about this. When I first told
her my plan, I don't think she believed me. But then after I used it
for the first time, she just laughed at me for a while.

@(the-end)
