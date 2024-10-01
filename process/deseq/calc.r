import_package("SummarizedExperiment", attach=TRUE)
import_package("dplyr", attach=TRUE)
.idmap = import('../idmap')
.gset = import('../../genesets')

#' Extract a data.frame with DE genes from a DESeq2 object
#'
#' @param mod   DESeq2 object
#' @param name  Name of the coefficient or contrast to extract
#' @return      A tibble with DE stats incl. 'ensembl_gene_id', 'label'
extract_result = function(mod, name) {
    if (grepl("_vs_", name) && !name %in% DESeq2::resultsNames(mod)) {
        contrast = name %>% sub("_", "@", .) %>% sub("_vs_", "@", .)
        contrast = strsplit(contrast, "@")[[1]]
        res = DESeq2::results(mod, contrast=contrast)
    } else {
        res = DESeq2::results(mod, name=name)
    }

    #todo: add shrinkage option
    as.data.frame(res) %>%
        tibble::as_tibble(rownames="ensembl_gene_id") %>%
        arrange(padj, pvalue) %>%
        mutate(label = .idmap$gene(ensembl_gene_id, to="hgnc_symbol"),
               label = ifelse(is.na(label), ensembl_gene_id, label)) %>%
        select(ensembl_gene_id, label, everything())
}

#' Test genes and gene sets for a given DESeq2 data set
#'
#' @param eset     DESeq2 object
#' @param design   Design formula for differential expression
#' @param extract  A regular expression of which resultsNames or contrasts to extract
#' @param contrast Whether `extract` refers to a contrast instead of a result name
#' @param on_error   Function to call if an error occurred (takes one parameter)
#' @return         A tibble with columns: term, genes[, sets]
genes = function(eset, design=DESeq2::design(eset), extract="^(?!Intercept)",
                 contrast=FALSE, on_error=function(e) stop(e)) {
    eset = clean_obj(eset, design)
    if (is.null(DESeq2::dispersions(eset))) {
        mod = DESeq2::DESeq(eset)
    } else {
        mod = DESeq2::nbinomWaldTest(eset)
    }

    if (!contrast)
        extract = grep(extract, DESeq2::resultsNames(mod), value=TRUE, perl=TRUE)

    tibble::tibble(term=extract) %>%
        mutate(genes = lapply(term, function(t)
            tryCatch(extract_result(mod, name=t), error=on_error)))
}

#' Test gene sets for a given DESeq2 data set
#'
#' @param res  Results data.frame from genes()
#' @param sets     A named list of gene set collections (lists of character vectors)
#' @param cl       A parallel cluster object or integer for number of cores
#' @return         A tibble with columns: term, genes[, sets]
sets = function(res, sets, cl=0) {
    if (length(sets) == 0)
        return(res);
    if (is.character(sets))
        sets = .gset$get_human(sets, drop=FALSE)
    if (!is.list(sets[[1]]))
        stop("'sets' must be a list of lists")
    if (is.null(names(sets)) || is.null(names(sets[[1]])))
        stop("'sets' parameter must be a named list of lists")

    do_cleanup = FALSE
    if (is.numeric(cl)) {
        do_cleanup = TRUE
        cl = clustermq::workers(cl, reuse=TRUE)
    }

    for (ns in names(sets)) {
        message("[process/deseq] Testing set: ", ns)
        test_fun = function(x, ...) if (is.null(x)) NULL else .gset$test_lm(x, ...)
        res[[ns]] = lapply(res$genes, test_fun, sets=sets[[ns]], cl=cl)
    }

    if (do_cleanup)
        cl$cleanup()
    res
}

#' Test genes and gene sets for a given DESeq2 data set
#'
#' @inheritParams  genes
#' @inheritParams  sets
#' @return         A tibble with columns: term, genes[, sets]
genes_and_sets = function(eset, design=DESeq2::design(eset), sets=list(),
            extract="^(?!Intercept)", contrast=FALSE, on_error=function(e) stop(e), cl=0) {
    res = genes(eset, design, extract, contrast, on_error)
    sets(res, sets, cl)
}

#' Clean a DESeq2 object and design
#'
#' This is removing sampes with NA values in design terms, unused factor
#' levels, and unused design terms
#'
#' @param eset     DESeq2 object
#' @param design   Design formula for differential expression
#' @return  A cleaned DESeq2 object
clean_obj = function(eset, design=DESeq2::design(eset)) {
    # remove samples where design value is NA
    drop_sample = rep(FALSE, ncol(eset))
    for (key in all.vars(design))
        drop_sample = drop_sample | is.na(colData(eset)[[key]])
    if (any(drop_sample)) {
        warning("Dropping sample(s): ",
                paste(colnames(eset)[drop_sample], collapse=", "), immediate.=TRUE)
        eset = eset[,!drop_sample]
    }

    # drop unused factor levels
    one_level = c()
    for (key in all.vars(design)) {
        val = colData(eset)[[key]]
        if (is.factor(val))
            colData(eset)[[key]] = droplevels(val)
        if (is.character(val))
            colData(eset)[[key]] = factor(val)
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
    eset
}
