MAKEFILES = $(shell find . -mindepth 2 -maxdepth 2 -name Makefile)
MDIRS = $(filter-out ./sys/,$(dir $(MAKEFILES)))
RSCRIPTS = $(wildcard *[^_].r)
RSCRIPTS_T = $(shell grep -l testthat $(RSCRIPTS) /dev/null)
RSCRIPTS_NO_T = $(filter-out $(RSCRIPTS_T),$(RSCRIPTS))

DEPS=$(shell ./dependencies.sh)
R_PKG=modules,$(shell Rscript -e 'cat(sub("package:", "", grep("^package:", search(), value=TRUE)), sep=",")')
Rscript = Rscript --default-packages=$(R_PKG)

.PHONY: test install-deps

define \n


endef

stats/nmf_mu.so:
	make -C stats nmf_mu.so

test:
	@$(foreach DIR,$(MDIRS),make -C $(DIR) test$(\n))
	$(if $(RSCRIPTS_NO_T), @echo "*** NO TESTS FOUND FOR: $(RSCRIPTS_NO_T) ***", )
	@$(foreach R,$(RSCRIPTS_T),echo $(R); $(Rscript) $(R)$(\n))

install-deps: dependencies.txt
	R -e "req = read.table('dependencies.txt', header=FALSE)[[1]]" \
	  -e "new = setdiff(req, installed.packages()[,'Package'])" \
	  -e "cat(new, \"\\n\")" \
	  -e "BiocManager::install(new)"

DESCRIPTION: DESCRIPTION.in dependencies.txt
	sed "s/^/\ \ \ \ /" dependencies.txt | sed 's/$$/,/' | sed -e "/@@@/r /dev/stdin" -e "/@@@/d" $< > $@

dependencies.txt: dependencies.sh
	bash $< > $@

print-%:
	@echo $* = $($*)
