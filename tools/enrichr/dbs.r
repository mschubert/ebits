import_package('dplyr', attach=TRUE)

#' List available databases as character vector
dbs = function() {
    req = httr::GET(url="http://maayanlab.cloud/Enrichr/datasetStatistics")

    if (!req$status == 200)
        stop("HTTP query error for database list")

    req$content %>%
        intToUtf8() %>%
        rjson::fromJSON() %>%
        `[[`('statistics') %>%
        bind_rows() %>%
        transmute(name = libraryName,
                  n_terms = numTerms,
                  n_genes = geneCoverage,
                  genes_per_term = genesPerTerm)
}

if (is.null(module_name())) {
    library(testthat)
    dbs = dbs()
    expect_is(dbs, "data.frame")
}
