import_package('dplyr', attach=TRUE)

#' Perform enrichment analysis
#'
#' @param genes  Character vector of gene names
#' @param db     Character string of database interfier
#' @return       Named (categories) list of genes (character vector)
run = function(genes, db) {
    url = function(...) paste0("http://amp.pharm.mssm.edu/Enrichr/", ...)
    genes = paste(genes, collapse="\n")

    req = httr::POST(url=url("enrich"), body=list(list=genes))
    if (!req$status == 200)
        stop("HTTP query error for POST on gene list")

    req = httr::GET(url("enrich?backgroundType=", db))
    if (!req$status == 200)
        stop("HTTP query error for retrieving enrichment results")

    req = httr::GET(url=url("export"),
                    query=list(file="API", backgroundType=db))

    if (!req$status == 200)
        stop("HTTP query error for retrieving enrichment table")

    req$content %>%
        intToUtf8() %>%
        textConnection() %>%
        read.table(sep="\t", header=TRUE, quote="") %>%
        transmute(term = Term,
                  overlap = Overlap,
                  p.value = P.value,
                  adj.p = Adjusted.P.value,
                  zscore = Z.score,
                  combined = Combined.Score,
                  genes = Genes)
}

if (is.null(module_name())) {
    library(testthat)

    genes = c("Nsun3", "Polrmt", "Nlrx1", "Sfxn5", "Zc3h12c",
              "Slc25a39", "Arsg", "Defb29", "Ndufb6")
    database = "Cancer_Cell_Line_Encyclopedia"

    result = run(genes, database)
    expect_is(result, "data.frame")
}
