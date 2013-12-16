.PHONY : test test-function

EMACS ?= emacs
LOADPATH ?= -L . -L test

test:
	$(EMACS) -Q -batch $(LOADPATH) -l test/function.el -f ert-run-tests-batch-and-exit

test-function:
	$(EMACS) -Q -batch $(LOADPATH) -l test/function.el -f ert-run-tests-batch-and-exit
