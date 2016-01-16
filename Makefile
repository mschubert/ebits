MAKEFILES = $(shell find . -mindepth 2 -maxdepth 2 -name Makefile)
MDIRS = $(dir $(MAKEFILES))
RSCRIPTS = $(wildcard *[^_].r)

R_PKG=modules,$(shell Rscript -e 'cat(sub("package:", "", grep("^package:", search(), value=TRUE)), sep=",")')
Rscript = Rscript --default-packages=$(R_PKG)

.PHONY: test

define \n


endef

test:
	@for DIR in $(MDIRS); do make -C $$DIR; done
	@$(foreach R,$(RSCRIPTS),echo $(R); $(Rscript) $(R)$(\n))

print-%:
	@echo $* = $($*)
