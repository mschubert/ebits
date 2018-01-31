seq = import('seq')

#' Create a blacklist from WGS read numbers
#'
#' @param ref_reads    GRanges object with reads (or character string of file)
#' @param chr_lengths  Numeric vector of chromosome lengths
#' @param chromosomes  Only include these chromosomes
#' @param lower        Lower quantile to cut off
#' @param upper        Upper quantile to cut off
#' @return             GRanges object of blacklisted regions
blacklist_from_ref = function(ref_reads, chr_lengths, lower=0.05, upper=0.999,
                              chromosomes=names(chr_lengths)) {
    assembly_df = data.frame(chromosome = names(chr_lengths), length=chr_lengths)
    if (is.character(ref_reads))
        ref_reads = seq$read_granges(ref_reads, assembly=assembly_df)

    # select bin size so avg >=100 reads/bin (down to 1kb), blacklist outliers
    bsize = round(100 * sum(as.numeric(chr_lengths)) / length(ref_reads))
    bsize = max(bsize, 1000)

    ref_bins = AneuFinder::fixedWidthBins(binsizes=as.integer(bsize),
        chromosomes=chromosomes, chrom.lengths=chr_lengths)[[1]]
    ref_counts = bin_reads(reads=ref_reads, bins=ref_bins)
    lower = ref_counts$counts < quantile(ref_counts$counts, lower)
    upper = ref_counts$counts > quantile(ref_counts$counts, upper)
    blacklist = ref_bins[lower | upper]
}
