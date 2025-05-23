# Run tests for arduino-cli-mode
#
# Time-stamp: <2025-05-23 19:13:22 tdw>

TESTS=customizable-variables-test.el

all: $(TESTS)

$(TESTS):
	emacs --batch -L .. -l ert -l $@ -f ert-run-tests-batch-and-exit

.PHONY: all $(TESTS)
# Makefile ends here
