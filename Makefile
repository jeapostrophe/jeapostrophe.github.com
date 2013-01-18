POSTS=posts/*
ATOM=atom.rkt
LIB=post.rkt post-help.rkt posts.scrbl
BLOG=blog.scrbl
TITLES=titles
CATS=categories

FLAGS=++xref-in setup/xref load-collections-xref --redirect-main "http://docs.racket-lang.org/"

.PHONY: all build preview remake clean deploy

all: build

build: blog/index.html blog/atom.xml

$(TITLES) blog/index.html: $(POSTS) $(BLOG) $(LIB)
	raco make $(BLOG)
	scribble --htmls --dest . $(FLAGS) $(BLOG)
	rm -f compiled/blog*
	raco make $(BLOG)
	scribble --htmls --dest . $(FLAGS) $(BLOG)

blog/atom.xml: $(ATOM) $(POSTS) $(TITLES)
	raco make $(ATOM)
	racket -t $(ATOM) -- $@

remake:
	rm -f blog/index.html

clean: remake
	rm -fr $(CATS) $(TITLES) posts/.auto*

deploy: build
	git add .
	git commit -m "Update" . || true
	git push
	git gc
	cd blog ; \
		git add . ; \
		git commit -m "Update" . || true ; \
		git push ; \
		git gc

preview: build
	rk ~/Dev/scm/github.jeapostrophe/exp/dir-serve.rkt blog
