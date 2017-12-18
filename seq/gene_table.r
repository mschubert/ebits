io = import('../io')

#' Creates a table of different identifiers and caches it
#'
#' @param dset  Ensembl data set, e.g. '{hsapiens,mmusculus}_gene_ensembl'
#' @param force  Re-generate table if it already exists
#' @return       A data.frame with gene and transcript-level information
gene_table = function(dset="hsapiens_gene_ensembl", force=FALSE) {
    fname = sprintf("gene_table-%s.RData", dset)
    cache = file.path(module_file(), "cache", fname)
    if (file.exists(cache) && !force)
        return(io$load(cache))

    mart = biomaRt::useMart(biomart="ensembl", dataset=dset)
    ids = c('external_gene_name', 'entrezgene', 'ensembl_gene_id',
            'band', 'chromosome_name', 'start_position', 'end_position',
            'ensembl_transcript_id', 'transcript_start', 'transcript_end',
            'transcription_start_site', 'strand', 'gene_biotype')
    mapping = biomaRt::getBM(attributes=ids, mart=mart)
    for (col in colnames(mapping)) {
        is_empty = nchar(as.character(mapping[,col])) == 0
        mapping[[col]][is_empty] = NA
    }

    dir.create(dirname(cache), showWarnings=FALSE)
    save(mapping, file=cache)
    mapping
}
