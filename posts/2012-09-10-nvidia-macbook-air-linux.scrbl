#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "post.rkt")

@title{Linux on a MacBook Air and Nvidia Projector Woes}
@categories["Linux" "Apple" "Nvidia"]

When I was quite young, in middle school, I switched from Windows to
Linux. I initially used Red Hat, but fairly quickly converted to
Debian. Eventually, when OS X first came out I was very excited and
switched to the Mac for my personal machines.

However, more recently, OS X has been becoming less and less Unix-like
and my tastes for Apple-ism having been reduce, so in December (after
classes were over), I switched back to Linux. First, I used Ubuntu and
then I switched to Arch.

However, running Linux on a laptop can be exciting, and a MacBook Air
with an Nvidia graphics card is particularly exciting. I had a brutal
problem with my projector setup, but I found the solution. In this
post, I lay out my tale of woe.

@(the-jump)

Since it's September again, it is time to teach class and I need to
project from my laptop. I'd been using multiple displays for a long
time with my Mac (I have an external screen in my office), so I
presumed that I had everything working correctly.

I use @link["http://willem.engen.nl/projects/disper/"]{disper} to
manage my display configurations. From Linux's perspective, my Air has
three displays: DFP-0 (the external display when it is connected with
DVI), DFP-1 (the external display when it is connected with VGA), and
DFP-2 (the built-in screen.) So I assumed it would be as simple as
typing @tt{disper -c} after connecting to clone my desktop across the two
displays.

Unfortunately, I got the error message: "Displays do not share a
common resolution". That's strange, I think, I distinctly remember
using these projectors last year, on OS X and getting a decently sized
screen (1024x768, I believe.) A quick @tt{disper -l} reveals that my
DFP-1 only has one resolution: "640x480".

This was right before class, so I just used that resolution only on
the projector and looked up at the screen when I needed to. Ugly, but
workable.

The next day I read all about XRandr and how you can add video modes
using @tt{gtf} and @tt{xrandr} and thought I had everything figured
out. When I went to connect again during a day I didn't teach, I had
an unbearable time of getting tons and tons of error messages and no
success.

I dealt with the small screen in class for a second day.

After that, I decided that I would connect with OS X on my laptop and
write down which video modes, resolutions, refresh rates, etc it could
handle so I could pass the correct arguments to @tt{gtf}. I was delighted
when I realized I'd be able to run the projector at 1400x1050 and my
laptop screen at 1440x900 and have them share a 1400x900 desktop. This
wasn't the default in OS X, but it was an option.

Booting back to Linux, I had a harrowing experience trying to override
the X server settings to get this mode enabled. Nothing.

At this point, I realized that I originally chose @tt{disper} rather than
@tt{xrandr} because Nvidia cards haven't always fully supported @tt{xrandr},
but used another "meta mode" system instead. I decided to use a meta
mode:

@commandline{nvidia-settings --assign CurrentMetaMode="DFP-2: 1440x900 { ViewPortIn=1400x900, ViewPortOut=1400x900+20+0 }, DFP-1: 1400x1050 { ViewPortIn=1400x900, ViewPortOut=1400x900+0+75 }"}

Unfortunately, this failed too, and would sometimes crash my X
server. But I was able to at least mirror the screen on the projector
on my laptop, but it was a small resolution (640x480) and it was
unscaled on my computer, so it was a tiny little box:

@commandline{nvidia-settings --assign CurrentMetaMode="DFP-2: 1440x900 { ViewPortIn=640x480, ViewPortOut=640x480+400+210 }, DFP-1: 640x480 { ViewPortIn=640x480, ViewPortOut=640x480+0+0 }"}

But I continued to try...

I inspected the logs and found that I got the error message "Unable to
read EDID for display device DFP-1". A quick run to Wikipedia told me
that EDID is the format for giving the valid display frequencies for
devices... so maybe OS X was reading it correctly but Linux wasn't?

I found an OS X app that would save EDID data and saved mine for the
projector (after booting into OS X.) Unfortunately after parsing it,
it said that it was fake data and that actually the EDID couldn't be
read. Defeated again.

This was a fruitful path though, because it made me discover the
@tt{ModeValidation} option in my X config where I could add
@tt{AllowNonEdidModes} so that the X server wouldn't insist on only using
modes that were given by the EDID block.

Failure.

Next, I tried to turn off other checks on valid modes:

@tt{NoHorizSyncCheck}

Failure.

@tt{NoVertRefreshCheck}

Failure.

@tt{NoDFPNativeResolutionCheck}

Failure.

I had almost given up. I decided to read through the entire Nvidia X
configuration manual and I discovered the @tt{ModeDebug} option that
would give detailed reasons for why certain modes were not allowed. I
turned that on, restarted X, and tried to use the big resolution...

The error message was "pixel clock exceeds maximum EDID pixel
clock". I don't know what a pixel clock is, but I looked in the manual
and found another mode validation override:

@tt{NoEdidMaxPClkCheck}

Success.

Now, I can use my projector at the full resolution. And it's
beautiful.

It's days like this that make you proud to be a Linux user. I've
subdued the computer and made it do exactly what I want. It feels
glorious.

The only remaining problems I have with the laptop are... the
microphone doesn't work and sometimes (maybe once every three weeks)
the X server will segfault, normally right after coming back from
sleep. This last thing is the worst because there's no way to reset
the video settings, so I have to reboot. The saga will continue...
