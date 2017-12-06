.genome = import('./genome')$genome

#' Get chromosome masks from assembly ID
#'
#' @param assembly  An assembly identifier, e.g. 'hg19' or 'GRCh38'
#' @param chromosomes  Only return the following chromosomes (default: all)
#' @param mask         Which masks to use. Available: AGAPS (assembly gaps),
#' AMB (intra-contig ambiguities),  RM (RepeatMasker), TRF (TandemRepeatFinder)
#' @return             A GRanges object of masked regions
assembly_mask = function(assembly_id, chromosomes=NULL,
                         mask=c("AGAPS", "AMB", "TRF")) {
    UseMethod("assembly_mask")
}

assembly_mask.character = function(assembly, chromosomes=NULL,
                         mask=c("AGAPS", "AMB", "TRF")) {
    if (assembly == "GRCh37") {
        assembly = "hg19"
        chromosomes = sprintf("chr%s", chromosomes)
        strip_chr_prefix = TRUE
    } else
        strip_chr_prefix = FALSE

    masked = .genome(assembly_id, masked=TRUE)

    if (length(chromosomes) == 0)
        chromosomes = names(masked)

    masked2granges = function(chr_name) {
        chr = masked[[chr_name]]
        IRanges::active(Biostrings::masks(chr)) = FALSE
        IRanges::active(Biostrings::masks(chr))[mask] = TRUE
        GenomicRanges::GRanges(seqnames = chr_name,
                               ranges = IRanges::ranges(as(chr, "XStringViews")))
    }
    ranges = lapply(chromosomes, masked2granges)
    ranges = suppressWarnings(do.call(c, ranges))
    GenomeInfoDb::seqinfo(ranges) = GenomeInfoDb::seqinfo(masked)

    if (strip_chr_prefix)
        GenomeInfoDb::seqlevels(ranges) = sub("^chr", "", GenomeInfoDb::seqlevels(ranges))

    ranges
}
