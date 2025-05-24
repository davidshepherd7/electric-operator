EMACS=emacs
CASK ?= cask

build :
	cask exec $(EMACS) -Q --batch --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" electric-operator.el

clean :
	@rm -f *.elc

test: unit 

unit: build build-unit
	${CASK} exec ert-runner

build-unit :
	cask exec $(EMACS) -Q --batch --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" test-helper.el test/*-test.el

benchmark: build
	${CASK} emacs -Q --batch -l ./electric-operator.elc -l ./electric-operator-benchmark.el

install:
	${CASK} install

.PHONY:	all test install clean build
