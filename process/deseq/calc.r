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
    # drop unused factor levels, record constant variables
    one_level = c()
    for (key in all.vars(design)) {
        val = colData(eset)[[key]]
        if (is.factor(val))
            colData(eset)[[key]] = droplevels(val)
        if (length(unique(val)) == 1)
            one_level = c(one_level, key)
    }

    # remove single-level factors and single-value integers from design
    rhs = stringr::str_trim(strsplit(as.character(design), "\\+")[[2]])
    rhs2 = sprintf("~ %s", paste(setdiff(rhs, one_level), collapse=" + "))
    if (length(one_level) > 0)
        warning("Dropping constant factors: ", paste(one_level, collapse=", "),
                " (new design: ", rhs2, ")", immediate.=TRUE)
    DESeq2::design(eset) = as.formula(rhs2)

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
