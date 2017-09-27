import_package('dplyr', attach=TRUE)

#' Get genes in a database
#'
#' @param db     Character string of database interfier
#' @return       Named (categories) list of genes (character vector)
genes = function(db) {
    url = function(...) paste0("http://amp.pharm.mssm.edu/Enrichr/", ...)

    if (length(db) != 1)
        stop("Length of 'db' must be 1")

    req = httr::GET(url("geneSetLibrary?libraryName=", db))
    if (!req$status == 200)
        stop("HTTP query error for gene list")

    re = req$content %>%
        intToUtf8() %>%
        rjson::fromJSON() %>%
        `[[`(1) %>%
        `[[`('terms') %>%
        lapply(function(x) names(unlist(x)))
}

if (is.null(module_name())) {
    library(testthat)
    genes = genes("Phosphatase_Substrates_from_DEPOD") # small DB
    expect_is(genes, "list")
    expect_is(genes[[1]], "character")
}
