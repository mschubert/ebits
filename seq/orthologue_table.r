io = import('../io')

#' Creates a table of different identifiers and caches it
#'
#' @param dset  Ensembl data set, e.g. '{hsapiens,mmusculus}_gene_ensembl'
#' @param force  Re-generate table if it already exists
#' @return       A data.frame with gene and transcript-level information
ortho_table = function(dset="hsapiens_gene_ensembl", to="mmusculus", force=FALSE) {
    fname = sprintf("orthologue_table-%s-%s.RData", dset, to)
    cache = file.path(module_file(), "cache", fname)
    if (file.exists(cache) && !force)
        return(io$load(cache))

    # workaround: useMart error: SSL certificate problem: unable to get local issuer certificate
    httr::set_config(httr::config(ssl_verifypeer = FALSE), override = FALSE)

    mart = biomaRt::useMart(biomart="ensembl", dataset=dset)
    ids = c('external_gene_name', 'ensembl_gene_id',
            paste0(to, '_homolog_ensembl_gene'),
            paste0(to, '_homolog_associated_gene_name'))
    mapping = biomaRt::getBM(attributes=ids, mart=mart)
    for (col in colnames(mapping)) {
        is_empty = nchar(as.character(mapping[,col])) == 0
        mapping[[col]][is_empty] = NA
    }

    dir.create(dirname(cache), showWarnings=FALSE)
    save(mapping, file=cache)
    mapping
}
