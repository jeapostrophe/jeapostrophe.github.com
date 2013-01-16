POSTS=posts/*
ATOM=atom.rkt
LIB=post.rkt post-help.rkt posts.scrbl
BLOG=blog.scrbl
TITLES=titles
CATS=categories

FLAGS=++xref-in setup/xref load-collections-xref --redirect-main "http://docs.racket-lang.org/"

all:blog/index.html blog/atom.xml

$(TITLES) blog/index.html: $(POSTS) $(BLOG) $(LIB)
	raco make $(BLOG)
	scribble --htmls --dest . $(FLAGS) $(BLOG)
	rm -f compiled/blog*
	raco make $(BLOG)
	scribble --htmls --dest . $(FLAGS) $(BLOG)

blog/atom.xml: $(ATOM) $(POSTS) $(TITLES)
	raco make $(ATOM)
	racket -t $(ATOM) -- $@

clean:
	rm -fr blog $(CATS) $(TITLES) posts/.auto*
