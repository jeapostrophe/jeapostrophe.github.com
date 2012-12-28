#lang scribble/manual
@(require (planet ryanc/scriblogify/scribble-util)
          (for-label racket/base
                     rackunit
                     racket/list))
@literal{
---
layout: post
title: "exec and Tail-call Optimization"
comments: true
categories:
- Systems
---
}

I'm often bothered by programs that fail to use @tt{exec} properly and
instead use @tt{system}. In this article, we'll review the difference
and relate it to tail-call optimization.

@(the-jump)

In Unix, there's not really a way to start a totally new
process. Instead, every process comes into being by another process
duplicating itself with fork(). The two processes are identical at
that point, except that the fork() call returns 0 to the child and the
child's PID to the parent. Using this information, the two can behave
differently.

Often, what the child will do is change the program entirely by
loading a system binary and executing its main function. That task is
taken care of by the exec function, which receives the path to the
binary, plus the arguments, and, optionally, the environment.

Most programming languages give you access to a function named exec
which is a wrapper for this functionality. Its also likely that they
will also give a function named system that behaves almost the
same. It's main difference is that it returns the exit code of the
program when it exits and it invokes the shell to parse the
command-line arguments and look up the binary's full path.

If your program calls system in tail-position, meaning that the
program does nothing with the exit code nor does anything else after
ward, then you are wasting memory. In particular, the memory of the
parent process which has nothing to do. You should have just exec'd,
not forked and then exec'd.

Here's an example:

@filebox["bad.sh"]{
@verbatim{
#!/bin/bash

x=${1:-10}

if [ $x -eq 0 ] ; then
    read
    echo done
else
    ./bad.sh $(expr $x - 1)
fi
}}

The process tree for this bad code looks like this:

@verbatim{
/bin/zsh
 \_ bash bad.sh
|   \_ bash bad.sh 9
|       \_ bash bad.sh 8
|           \_ bash bad.sh 7
|               \_ bash bad.sh 6
|                   \_ bash bad.sh 5
|                       \_ bash bad.sh 4
|                           \_ bash bad.sh 3
|                               \_ bash bad.sh 2
|                                   \_ bash bad.sh 1
|                                       \_ bash bad.sh 0
 \_ ps f
}

Compared to:

@filebox["good.sh"]{
@verbatim{
#!/bin/bash

x=${1:-10}

if [ $x -eq 0 ] ; then
    read
    echo done
else
    exec ./good.sh $(expr $x - 1)
fi
}}

(Notice that line 9 is different---we've explicitly used exec.)

This good code has a process tree like:

@verbatim{
/bin/zsh
 \_ bash good.sh 0
 \_ ps f
}

This is very similar to the concept of safe-for-space, or tail-call
optimization, in programming languages. As you can see, unfortunately
bash is not safe-for-space by default. That is, it doesn't keep track
of when a call is in tail-position and automatically use exec rather
than system.

It's not just a problem with bash either, I've never known any shell
that can run this program correctly.

In most cases, this is not problematic because the stack is unlikely
to grow very large and the executed program is unlikely to run for a
long time. However, it most often shows up as a problem with X11
window managers and menu programs.

Your Xsession initialization should always exec your window manager,
because there's nothing else it needs to do afterward.

An X11 menu program should also use exec to run the program, otherwise
whenever you start, for example Emacs, the shell that started it will
persist for the entire time you are running Emacs (presumably the
entire time you are at the computer.) In addition, you should exec
your menu program so that the shell that starts it is replaced as
well.

For example, the default Xmonad configuration does not do this
correctly and will invoke dmenu without an exec, leaving around the
shell forever. (dmenu is programmed correctly, though.)

So, raise your right arm and say with me: "I will always exec in
tail-position!"
