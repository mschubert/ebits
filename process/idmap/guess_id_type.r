#' Guesses the ID type from a character vector of IDs
#'
#' @param from_ids  Character vector of IDs
#' @return          Character string describing the ID type
guess_id_type = function(from_ids) {
    if (sum(grepl("^ENSG", from_ids)) > length(from_ids)/2)
        'ensembl_gene_id'
    else if (sum(grepl("_at$", from_ids)) > length(from_ids)/2)
        'affy'
    else if (sum(grepl("^ILMN", from_ids)) > length(from_ids)/2)
        'illumina'
    else if (sum(suppressWarnings(!is.na(as.numeric(from_ids)))) > length(from_ids)/2)
        'entrezgene'
    else if (sum(grepl("^[A-Z]{1,3}[0-9]{5,6}$", from_ids)) > length(from_ids)/2)
        'genbank'
    else if (sum(grepl("^[A-Z]+[0-9]?$", from_ids)) > length(from_ids)/3)
        'hgnc_symbol'
    else
        stop("Can not guess ID type: ", paste(head(from_ids), collapse=", "), " ...")
}
