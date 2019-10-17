#' Guesses the ID type from a character vector of IDs
#'
#' @param from_ids  Character vector of IDs
#' @return          Character string describing the ID type
id_type = function(from_ids) {
    if (sum(grepl("^(ENS[A-Z]*G)", from_ids)) > length(from_ids)/2)
        'ensembl_gene_id'
    else if (sum(grepl("_at$", from_ids)) > length(from_ids)/2)
        'affy'
    else if (sum(grepl("^ILMN", from_ids)) > length(from_ids)/2)
        'illumina'
    else if (sum(suppressWarnings(!is.na(as.numeric(from_ids)))) > length(from_ids)/2)
        'entrezgene'
    else if (sum(grepl("^[A-Z]{1,3}[0-9]{5,6}$", from_ids)) > length(from_ids)/2)
        'genbank'
    else if (sum(grepl("^[A-Z]+[0-9]{1,2}?$", from_ids)) > length(from_ids)/3)
        'hgnc_symbol'
    else if (sum(grepl("^[A-Z][a-z]+[0-9]{1,2}?$", from_ids)) > length(from_ids)/3)
        'mgi_symbol'
    else
        stop("Can not guess ID type: ", paste(head(from_ids), collapse=", "))
}

#' Guess Ensembl data set for ID types
#'
#' @param from_ids  Character vector of IDs
#' @return          Character string describing the data set
dset = function(from_ids) {
    id_type = tryCatch(id_type(from_ids), error = function(e) "unknown")
    if (id_type == "mgi_symbol")
        "mmusculus_gene_ensembl"
    else
        "hsapiens_gene_ensembl"
}

if (is.null(module_name())) {
    library(testthat)

    expect_equal(id_type("TP53"), "hgnc_symbol")
    expect_equal(id_type("Trp53"), "mgi_symbol")

    expect_equal(dset("TP53"), "hsapiens_gene_ensembl")
    expect_equal(dset("Trp53"), "mmusculus_gene_ensembl")
}
