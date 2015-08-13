MAKEFILES = $(shell find . -mindepth 2 -name Makefile)
MDIRS = $(shell dirname $(MAKEFILES))

.PHONY: test

test:
	for DIR in $(MDIRS); do make -C $$DIR; done
