import_package('dplyr', attach=TRUE)

#' Get genes in a database
#'
#' @param db     Character string of database interfier
#' @return       Named (categories) list of genes (character vector)
genes = function(db) {
    cache = file.path(module_file(), "cache", paste0(db, ".rds"))
    if (file.exists(cache))
        return(readRDS(cache))

    url = function(...) paste0("https://maayanlab.cloud/Enrichr/", ...)

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

    dir.create(dirname(cache), showWarnings=FALSE)
    saveRDS(re, file=cache)
    re
}

if (is.null(module_name())) {
    library(testthat)
    genes = genes("Phosphatase_Substrates_from_DEPOD") # small DB
    expect_is(genes, "list")
    expect_is(genes[[1]], "character")
}
