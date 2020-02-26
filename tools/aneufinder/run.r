import_package('dplyr', attach=TRUE)
bin_reads = import('./bin_reads')$bin_reads
blacklist_from_ref = import('./blacklist_from_ref')$blacklist_from_ref
seq = import('../../seq')

#' Run aneufinder with clean output structure
#'
#' Does not support strandseq so far
#'
#' @param files          Character vector of BAM or BED files, or directory
#' @param assembly       Character string identifying the assembly; e.g.
#'                       GRCh38, mm10, etc.; sequence lengths derived from that
#' @param chromosomes    Restrict analysis to the following chromosomes
#' @param blacklist      BED file name with blacklisted regions, BAM file with
#'   reads to derive blacklist from, or NULL if no blacklist
#' @param binsize        Average bin size for reads
#' @param correction     "GC", "mappability" or both in a vector
#' @param states         Max number of chromosome copies to model
#' @param bin_width_ref  Reference BAM file for creating variable width bins
#'   value can either be NULL (fixed bins) or a path to a file to use for reference
#' @param method         Aneufinder method: "edivisive", "HMM" or "dnacopy"
#' @return               A list of models, one for each input file
run = function(files, assembly, chromosomes=NULL, blacklist=NULL,
               binsize=1e6, correction="GC", states=10,
               mappability_reference=NULL, paired_reads=FALSE,
               duplicate_reads=FALSE, min_mapq=10, max_time=-1, n_trials=15,
               bin_width_ref=NULL, method="edivisive") {

    stopifnot(length(files) >= 1)
    stopifnot(length(binsize) == 1) # change [[1]] below if removing

    if (length(files) == 1 && file.info(files)[1,"isdir"])
        files = list.files(files, pattern="\\.(bam|bed)(\\.gz)?$",
                           full.names=TRUE)

    # get assembly information
    genome = seq$genome(assembly)
    chr_lengths = seq$chr_lengths(genome)
    if (is.null(chromosomes))
        chromosomes = grep("^((chr)?[0-9]+|X|Y)$", names(chr_lengths), value=TRUE)
    assembly_df = data.frame(chromosome = names(chr_lengths), length=chr_lengths)

    # partition the reads into bins
    if (!is.null(bin_width_ref)) {
        ref_reads = seq$read_granges(bin_width_ref, assembly=assembly_df)

        if (!is.null(blacklist) && grepl("\\.bam$", blacklist))
            blacklist = blacklist_from_ref(ref_reads, chr_lengths,
                                           chromosomes=chromosomes)

        bins = AneuFinder::variableWidthBins(ref_reads, binsizes=binsize,
            chromosomes = chromosomes)[[1]]
    } else
        bins = AneuFinder::fixedWidthBins(chromosomes = chromosomes,
            chrom.lengths = chr_lengths, binsizes = binsize)[[1]]

    # load BAM or BED files to granges objects
    reads = seq$read_granges(as.list(files), chromosomes=chromosomes,
                             pairedEndReads=paired_reads, assembly=assembly_df,
                             remove.duplicate.reads=!duplicate_reads,
                             min.mapq=min_mapq, blacklist=blacklist)

    binned_reads = mapply(bin_reads, reads=reads, id=basename(files),
                          MoreArgs = list(bins=bins, min_mapq=min_mapq))

    # gc or mappability correction #TODO: preserve order
    if ("GC" %in% correction) {
        binned_reads = AneuFinder::correctGC(binned_reads, GC.BSgenome=genome,
                                             same.binsize=TRUE)
    }
    if ("mappability" %in% correction)
        binned_reads = AneuFinder::correctMappability(binned_reads,
            is.null(bin_width_ref), mappability_reference, chr_lengths)

    # run HMM/dnacopy
    models = lapply(binned_reads, function(br) {
        m = try(AneuFinder::findCNVs(br,
                eps = 0.1,
                max.time = max_time,
                max.iter = 5000,
                num.trials = n_trials,
                states = c("zero-inflation", sprintf("%i-somy", 0:states)),
                most.frequent.state = "2-somy",
                method = method))
        if (class(m$segments) == "GRanges") {
            idx = unique(as.character(GenomeInfoDb::seqnames(m$segments)))
            GenomeInfoDb::seqinfo(m$segments) = GenomeInfoDb::seqinfo(genome)[idx]
        }
        m
    })
}
