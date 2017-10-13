#' Lists all gene sets for a collection
#'
#' @param db  Identifier for a collection
#' @return    List of character vectors
genes = function(db, type="hgnc_symbol", species="human") {
    # be consistent with using biomart idenfiers
    types = setNames(c("symbols", "entrez"), c("hgnc_symbol", "entrez_gene_id"))

    gs = msigdb::msigdb.genesets(db, types[type], species=species)$genesets
    names(gs) = sub(paste0(db, ":"), "", names(gs))
    gs
}
