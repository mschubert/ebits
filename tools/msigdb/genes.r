#' Lists all gene sets for a collection
#'
#' @param db  Identifier for a collection
#' @return    List of character vectors
genes = function(db, type="hgnc_symbol", species="human") {
    stopifnot(type == "hgnc_symbol", species == "human")
    gs = msigdb::subsetCollection(msigdb::getMsigdb(), subcollection=db)
    re = lapply(gs, function(g) g@geneIds)
    names(re) = sapply(gs, function(g) g@setName)
    re
}
