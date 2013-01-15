#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{DrDr and its archives}
@categories["Racket" "Systems"]

After I developed @link["http://drdr.racket-lang.org"]{DrDr}, it was running
smoothly for a couple hundred revisions, when suddenly it stopped
working. I investigated and found that I couldn't create any
files... had I really run out of space on the 220G hard drive?

In this post, I discuss how I found and fixed the problem.

@(the-jump)

@section{Background on DrDr}

DrDr is an continuous integration system for Racket. Every time a push
is made to our repository, DrDr will download it, compile it,
and "test" every single file in the code base. In this case, "test"
just means compile, load, and run. Some files, such as
@tt{collects/tests/web-server/run-all-tests.rkt} are test suites that
run a huge number of tests, whereas others, like
@tt{collects/xml/xml.rkt} are simply implementations that have no
run-time behavior. DrDr records data about every file's run---how long
it took, what the output was, what the exit code was, etc---and
reports it to the Racket community. (This has turned out to be a
really good idea because DrDr needs virtually no cooperation with the
files being tested. Racket developers are free to write tests of any
kind with any library. They just have to ensure that failures are
written to STDERR and/or the file exits with a code other than 0.)

In all, DrDr tests about 8,700 different files and runs for about an
hour and ten minutes per push. (It's a 12-core machine and gets about
4x parallel speed-up from that.)

DrDr is designed to be extremely efficient and crash-resistant. It
uses the filesystem as a database and associated two data files with
every source file (and directory). The first is a log of the recorded
information and the second is an "analysis" of the log that determines
how many changes, unclean exits, timeouts, etc there were in the
file (or directory.) This allows the Web view to simply read one file
and print out the analysis data without doing any search. (The
crash-resistance comes from it using these files (or rather their
lack) as a work list of things to do. It's almost like a giant
Makefile that creates and records these files as it goes. So when
there's a crash, there's no in-memory state that needs to be saved or
recovered... it's all written to the filesystem immediately.)

@section{Running out of space...}

This means that on every push to our repository, DrDr creates about
18,000 files. Every file is very small, typically less than 50 bytes,
because most "tests" have no errors and no output. There's no danger
of running out of space like that... right?

If you know anything about file-systems, you should know that there's
a thing called on "inode". When I explain it to students, I say this:
your file-system is like a giant filing cabinet, it has a finite
amount of volume it can organize and a finite number of folders to put
things in, you can run out of either and your file-system won't be
usable. The volume is the space (220G) and the folders are the
inodes---the names for file blocks. On my ext4 partition, I have 14M
inodes for that 220G space.

After a few hundred revisions, I had run out of inodes and was in
trouble.

@section{Saving space}

Most archive and compression formats are designed for saving
space. They normally also save inodes... because 10,000 files can be
put into 1 zip file... but that's not normally their primary
purpose. (Naturally, Unix tradition has done a great job of separating
these tasks: tar turns many files into one that can be compressed
independently.)

Based on that, I thought of just tarring the files and reading the tar
file to find the file data when the Web application requested
it. Unfortunately, tar was not designed for easy access to arbitrary
files.

A tar file is basically a linked list where one file's header contains
a link to the next file (actually the file size, but that's also a
pointer to the next block.) So if you are looking for a particular
file in the archive, you have to do an O(n) search. In DrDr's case, n
is 18,000.

Other popular formats, such as zip or DAR, are just a little bit
better. They have a single catalog for the entire archive---a list
of the files with pointers to their location in the archive. Still an
O(n) search, but at least it plays better with the block cache by
limiting seeking, etc.

After a brief investigation of other formats and failing to find any
efficient format, I decided to write my own. Essentially, I needed
something more like a filesystem.

@section{DrDr's Archive Format}

The
@link["https://github.com/plt/racket/blob/master/collects/meta/drdr/archive.rkt"]{entire
archive code} is a mere 150 lines of code.

I had the advantage of not needing to keep track of permission or any
UNIX attributes. In addition, I didn't need to deal with modifying the
files, just recording them.

The entire process of turning a directory into an archive is 28
lines. Here's how it works: 

- Think of the file as a heap of values in some order

- Go through all files and directories and append them one after
another in this heap.

- Encode directories as hash tables that map their contents' paths to
where they start and how long they are.

- Ensure that you write directories after their contents (so you
actually know where the files will start)

- Write out the root directory hash as the first thing

And that's it. It's a very obvious encoding of a filesystem. It gives
us O(log n) number of accesses to find the file. All but one of those
will be the reading of a Racket hash table that has another O(log
n) (where n is the number of files in that directory) number of
operations until we find the sub-directory.

The code that does the lookup is a bit longer---47 lines---because it
has quite a lot of error handling in case there's some sort of
problem.

@section{The outcome}

After implementing this, it was a simple matter to write a script to
archive everything that had been produced. (I deleted the last
revision to give me enough inodes to work with.) I decided to only use
this archive format for pushes other than the first one hundred.

This worked really nice. (Although, unfortunately, about a year ago I
had a hard-drive crash and couldn't recover the data. Not super
important, but a little annoying.)

In the past year, we've had 2,111 pushes, generated 75 G of data, with
about 31 M per archived push and 150 M per non-archived push.

It's experiences with beautiful, simple code like this, that make me
say: I love Racket.
