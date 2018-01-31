seq = import('../../seq')

#' Bin reads into segments with read counts
#'
#' @param reads    GRanges object of aligned reads for all files
#' @param binsize  Width of bins to assign reads to (default: 1000)
#' @return         A list of GRanges objects representing the aligned read counts
bin_reads = function(reads, binsize=1000) {
    bins = chromstaR::fixedWidthBins(
        chrom.lengths = seq$lengths(reads[[1]]),
        chromosomes = levels(seq$names(reads[[1]])),
        binsizes = binsize)

    lapply(reads, chromstaR::binReads, assembly=NULL,
        binsizes=NULL, reads.per.bin=NULL, stepsize=binsize/2, bins=bins)
}
