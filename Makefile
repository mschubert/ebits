MAKEFILES = $(shell find . -mindepth 2 -maxdepth 2 -name Makefile)
MDIRS = $(dir $(MAKEFILES))
RSCRIPTS = $(wildcard *[^_].r)
RSCRIPTS_T = $(shell grep -l testthat $(RSCRIPTS) /dev/null)
RSCRIPTS_NO_T = $(filter-out $(RSCRIPTS_T),$(RSCRIPTS))

DEPS=$(shell ./dependencies.sh)
R_PKG=modules,$(shell Rscript -e 'cat(sub("package:", "", grep("^package:", search(), value=TRUE)), sep=",")')
Rscript = Rscript --default-packages=$(R_PKG)

.PHONY: test deps

define \n


endef

test:
	@$(foreach DIR,$(MDIRS),make -C $(DIR) test$(\n))
	$(if $(RSCRIPTS_NO_T), \
			@echo "*** NO TESTS FOUND FOR: $(RSCRIPTS_NO_T) ***", )
	@$(foreach R,$(RSCRIPTS_T),echo $(R); $(Rscript) $(R)$(\n))

deps: dependencies.txt
	R -e "source('https://bioconductor.org/biocLite.R')" \
	  -e "req = read.table('dependencies.txt', header=FALSE)[[1]]" \
	  -e "new = setdiff(req, installed.packages()[,'Package'])" \
	  -e "cat(new, "'\\n'")" \
	  -e "biocLite(new)"

dependencies.txt: dependencies.sh
	bash $< > $@

print-%:
	@echo $* = $($*)
