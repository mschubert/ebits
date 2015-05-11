MAKEFILES = $(shell find . -name Makefile)

.PHONY: test

test:
	make -C $(MAKEFILES)
