#' Subset a BSgenome object using only the sequences provided
#'
#' @param genome    BSgenome object
#' @param seqnames  Name of sequences to subset
#' @return          BSgenome object with only the sequences requested
subset_genome = function(genome, seqnames) {
    stopifnot(all(seqnames %in% GenomeInfoDb::seqnames(genome)))
    genome@user_seqnames = setNames(seqnames, seqnames)
    genome@seqinfo = genome@seqinfo[seqnames]
    genome
}
