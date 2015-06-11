EMACS=emacs
CASK ?= cask

build :
	cask exec $(EMACS) -Q --batch --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" *.el

clean :
	@rm -f *.elc

test: build unit ecukes

ecukes: build
	${CASK} exec ecukes

install:
	${CASK} install

.PHONY:	all test unit ecukes install clean build
