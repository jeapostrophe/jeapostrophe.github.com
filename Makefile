POSTS=posts/*
ATOM=atom.rkt
LIB=post.rkt post-help.rkt posts.scrbl
BLOG=blog.scrbl

FLAGS=++xref-in setup/xref load-collections-xref --redirect-main "http://docs.racket-lang.org/"

all:blog/index.html blog/atom.xml

blog/index.html: $(POSTS) $(BLOG) $(LIB)
	raco make $(BLOG)
	scribble --htmls --dest . $(FLAGS) $(BLOG)

blog/atom.xml: $(ATOM) $(POSTS)
	raco make $(ATOM)
	racket -t $(ATOM) -- $@
