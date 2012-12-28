POSTS=posts/*
BLOG=blog.scrbl

FLAGS=++xref-in setup/xref load-collections-xref --redirect-main "http://docs.racket-lang.org/"

build/index.html: $(POSTS) $(BLOG)
	raco make $(BLOG)
	scribble --htmls --dest . $(FLAGS) $(BLOG)
