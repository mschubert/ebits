#' Assign reads to pre-existing bins
#'
#' This function exists in AneuFinder, but it is re-bins all reads and ignores
#' already provided reads. The functionality here is basically the same as:
#' https://github.com/ataudt/aneufinder/blob/master/R/binReads.R#L269-L303
#'
#' @param reads     A GRanges object of reads
#' @param bins      A GRanges object of bins
#' @param min_mapq  Min mapping quality (added to object for info)
#' @param id        Sample ID (added to object for info)
#' @return          A GRanges object of the read counts per bin
bin_reads = function(reads, bins, min_mapq=NULL, id=NULL) {
    grc = function(r) GenomicRanges::countOverlaps(bins, r)
    S4Vectors::mcols(bins) = S4Vectors::DataFrame(
        counts = grc(reads),
        mcounts = grc(reads[BiocGenerics::strand(reads) == '-']),
        pcounts = grc(reads[BiocGenerics::strand(reads) == '+']))

    qualityInfo = list(complexity = c(MM=NA),
                       coverage = NA,
                       spikiness = AneuFinder:::qc.spikiness(bins$counts),
                       entropy = AneuFinder:::qc.entropy(bins$counts))

    attr(bins, 'qualityInfo') = qualityInfo
    attr(bins, 'min.mapq') = min_mapq
    attr(bins, 'ID') = id
    bins
}
