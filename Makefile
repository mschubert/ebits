MAKEFILES = $(shell find . -mindepth 2 -maxdepth 2 -name Makefile)
MDIRS = $(dir $(MAKEFILES))
RSCRIPTS = $(wildcard *[^_].r)
RSCRIPTS_T = $(shell grep -l testthat $(RSCRIPTS) /dev/null)
RSCRIPTS_NO_T = $(filter-out $(RSCRIPTS_T),$(RSCRIPTS))

R_PKG=modules,$(shell Rscript -e 'cat(sub("package:", "", grep("^package:", search(), value=TRUE)), sep=",")')
Rscript = Rscript --default-packages=$(R_PKG)

.PHONY: test

define \n


endef

test:
	@for DIR in $(MDIRS); do make -C $$DIR; done
	$(if $(RSCRIPTS_NO_T), \
			@echo "*** NO TESTS FOUND FOR: $(RSCRIPTS_NO_T) ***", )
	@$(foreach R,$(RSCRIPTS_T),echo $(R); $(Rscript) $(R)$(\n))

print-%:
	@echo $* = $($*)
