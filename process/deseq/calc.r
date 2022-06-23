import_package("SummarizedExperiment", attach=TRUE)
import_package("dplyr", attach=TRUE)
.idmap = import('../idmap')
.gset = import('../../genesets')

#' Extract a data.frame with DE genes from a DESeq2 object
#'
#' @param mod  DESeq2 object
#' @param rn   Name of the coefficient to extract
#' @return     A tibble with DE stats incl. 'ensembl_gene_id', 'label'
extract_result = function(mod, rn) {
    DESeq2::results(mod, name=rn) %>%
        as.data.frame() %>%
        tibble::as_tibble(rownames="ensembl_gene_id") %>%
        arrange(padj) %>%
        mutate(label = .idmap$gene(ensembl_gene_id, to="hgnc_symbol")) %>%
        select(ensembl_gene_id, label, everything())
}

#' Test genes and gene sets for a given DESeq2 data set
#'
#' @param eset     DESeq2 object
#' @param design   Design formula for differential expression
#' @param sets     A named list of gene set collections (lists of character vectors)
#' @param extract  A regular expression of which resultsNames to extract
#' @return         A tibble with columns: term, genes[, sets]
genes_and_sets = function(eset, design=DESeq2::design(eset), sets=list(), extract="^(?!Intercept)") {
    DESeq2::design(eset) = design
    for (v in all.vars(design))
        if (is.factor(colData(eset)[[v]]))
            colData(eset)[[v]] = droplevels(colData(eset)[[v]])

    mod = DESeq2::DESeq(eset)
    res = tibble::tibble(term=grep(extract, DESeq2::resultsNames(mod), value=TRUE, perl=TRUE)) %>%
        mutate(genes = lapply(term, extract_result, mod=mod))

    if (is.character(sets))
        sets = .gset$get_human(sets, drop=FALSE)
    if (is.null(names(sets)))
        stop("'sets' parameter must be a named list")

    for (ns in names(sets)) {
        message("[process/deseq] Testing set: ", ns)
        res[[ns]] = lapply(res$genes, .gset$test_lm, sets=sets[[ns]])
    }

    res
}
