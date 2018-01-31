seq = import('../../seq')

#' Run chromstar using a clean API and no side effects
#'
#' @param exp_table    A data.frame or tabular file with fields:
#'    'file': bam file with the mark we are investigating
#'    'mark': the ChIP mark that we are investigating
#'    'condition': sample identifier or e.g. basal vs perturbed
#'    'replicate': number of replicate condition (all '1' if no replicates)
#'    'pairedEndReads': whether fibrary is paired end (TRUE/FALSE)
#'    'controlFiles': bam file for the input DNA
#' @param assembly     Character string identifying the assembly; e.g.
#'    GRCh38, mm10, etc.; sequence lengths derived from that
#' @param chromosomes  Restrict analysis only to those chromosomes listed
#' @param duplicate_reads  Whether to allow duplicate reads (default: FALSE)
#' @param min_mapq     Minimum mapping quality (default: 10)
#' @return             A list of GRanges objects for all aligned reads
read_files = function(exp_table, assembly, chromosomes=NULL, duplicate_reads=FALSE,
                      min_mapq=10, blacklist=NULL) {
    files = unique(c(exp_table$file, exp_table$controlFiles))
    paired_end = unique(c(exp_table$pairedEndReads))
    if (length(paired_end) != 1)
        stop("pairedEndReads must be common to experiment")

    genome = seq$genome(assembly)
    chr_lengths = seq$chr_lengths(genome)
    if (is.null(chromosomes))
        chromosomes = grep("^((chr)?[0-9]+|X)$", names(chr_lengths), value=TRUE)
    assembly_df = data.frame(chromosome = names(chr_lengths), length=chr_lengths)

    # get and bin reads
    reads = seq$read_granges(as.list(files), chromosomes=chromosomes,
                             pairedEndReads=paired_end, assembly=assembly_df,
                             remove.duplicate.reads=!duplicate_reads,
                             min.mapq=min_mapq, blacklist=blacklist)
    names(reads) = files
    for (i in seq_along(reads))
        .seqinfo(reads[[i]]) = seq$info(genome)
    reads
}
