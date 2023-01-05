import_package("dplyr", attach=TRUE)
.idmap = import('../process/idmap')
.guess = import('../process/idmap/guess')
.filter = import('../genesets/filter')$filter

#' Use a Fisher's Exact test for enriched categories in a set of genes
#'
#' @param valid  All genes that are considered
#' @param hits   Genes that are considered hits
#' @param sets   List of character vectors
#' @param min    The minimum number of genes in a list to keep the list
#' @param max    The maximum number of genes in a list to keep the list
#' @return       A data.frame with association results
test_fet = function(valid, hits, sets, min=2, max=Inf) {
    test_one = function(valid, hits, set) {
        mat = matrix(ncol=2,
            c(length(setdiff(valid, set)), length(setdiff(set, hits)),
              length(setdiff(hits, set)), length(intersect(hits, set)))
        )

        broom::tidy(fisher.test(mat)) %>%
            select(-method, -alternative) %>%
            mutate(size = length(set),
                   size_used = length(intersect(valid, set)))
    }

    msg = c()

    all_sets = unique(unlist(sets))
    if (mean(all_sets %in% valid) < 0.2) {
        idt = .guess$id_type(all_sets)
        message("[geneset/test] low identifier overlap, mapping genes to ", sQuote(idt))
        valid = .idmap$gene(valid, to=idt)
    }

    sets = .filter(sets, valid=valid, min=min, max=max)
    lapply(sets, test_one, valid=valid, hits=hits) %>%
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

    sets = list(set=letters[1:10])
    valid = letters
    hits = letters[2:11]

    res = test_fet(valid, hits, sets)

    expect_equal(res$label, "set")
    expect_equal(res$size, 10)
    expect_equal(res$size_used, 10)
    expect_gt(res$estimate, 1)
}
