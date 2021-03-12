import_package("dplyr", attach=TRUE)
.idmap = import('../process/idmap')
.guess = import('../process/idmap/guess')

#' Use a linear model to test consistent gene DE in a collection of gene sets
#'
#' @param genes  A data.frame with gene separation statistics
#' @param sets   List of character vectors
#' @param label  Column name of gene names
#' @param stat   Column name of separation statistics
#' @return       A data.frame with association results
test = function(genes, sets,
                label=c("external_gene_name", "gene", "name", "gene_name", "ensembl_gene_id"),
                stat=c("stat", "statistic")) {
    test_one = function(res, set) {
#        tryCatch({
            res %>%
                mutate(in_set = !! slab %in% set + 0) %>%
                lm(as.formula(paste(stat, "~ in_set")), data=.) %>%
                broom::tidy() %>%
                filter(term == "in_set") %>%
                select(-term) %>%
                mutate(size = sum(res[[label]] %in% set),
                       size_used = sum(res[[label]] %in% set & !is.na(res[[stat]])))
#        }, error = function(e) data.frame(estimate = NA))
    }

    if (length(label) > 1) {
        label = intersect(label, colnames(genes))[1]
        first = head(na.omit(genes[[label]]), 2) %>% sQuote() %>% paste(collapse=", ")
        message("[geneset/test] column ", sQuote(label), " for as gene identifier (", first, ", ...)")
    }
    slab = rlang::sym(label)
    if (length(stat) > 1) {
        stat = intersect(stat, colnames(genes))[1]
        message("[geneset/test] column ", sQuote(stat), " as separation statistic")
    }

    lapply(sets, test_one, res=genes) %>%
        setNames(names(sets)) %>%
        dplyr::bind_rows(.id="label") %>%
        as_tibble() %>%
        na.omit() %>%
        select(label, size, size_used, everything()) %>%
        mutate(adj.p = p.adjust(p.value, method="fdr")) %>%
        arrange(adj.p, p.value)
}

if (is.null(module_name())) {
    library(testthat)

    genes = data.frame(gene = LETTERS[1:10], stat=1:10)
    sets = list(a=LETTERS[1:5])
    res = test(genes, sets)

    expect_true(inherits(res, "data.frame"))
    expect_equal(res$estimate, -5)
}
