seq = import('../../seq')

`.seqinfo<-` = function(x, value) {
    idx = unique(as.character(GenomeInfoDb::seqnames(x)))
	x@seqinfo = value[idx]
	if (inherits(x, "GRanges"))
		x@seqnames = droplevels(x@seqnames)
	x
}

#' Bin reads into segments with read counts
#'
#' @param reads    GRanges object of aligned reads for all files
#' @param binsize  Width of bins to assign reads to (default: 1000)
#' @return         A list of GRanges objects representing the aligned read counts
bin_reads = function(reads, binsize=1000) {
	# make sure all models have the same genome
	sinfo = unique(lapply(reads, seq$info))
	if (length(sinfo) != 1)
		stop("reads must all have same seqinfo: ", paste(sinfo, collapse=", "))

	# this always creates a list of bins, length 1 because we request 1 size
    bins = chromstaR::fixedWidthBins(
        chrom.lengths = seq$lengths(reads[[1]]),
        chromosomes = levels(seq$names(reads[[1]])),
        binsizes = binsize)
	stopifnot(is.list(bins) && length(bins) == 1)
	.seqinfo(bins[[1]]) = sinfo[[1]]

	# stepsize is acutally added to 'counts' mcol, not created as separate bin
    lapply(reads, chromstaR::binReads, assembly=NULL,
        binsizes=NULL, reads.per.bin=NULL, stepsize=binsize/2, bins=bins)
}
