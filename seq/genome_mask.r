.genome = import('./genome')$genome

#' Get chromosome masks from assembly ID
#'
#' @param assembly  An assembly identifier, e.g. 'hg19' or 'GRCh38'
#' @param chrs      Only return the following chromosomes (default: all)
#' @param mask      Which masks to use. Available: AGAPS (assembly gaps),
#' AMB (intra-contig ambiguities),  RM (RepeatMasker), TRF (TandemRepeatFinder)
#' @return          A GRanges object of masked regions
genome_mask = function(assembly, chrs=NULL,
                         mask=c("AGAPS", "AMB", "TRF")) {
    UseMethod("genome_mask")
}

genome_mask.character = function(assembly, chrs=NULL,
                         mask=c("AGAPS", "AMB", "TRF")) {
    if (assembly == "GRCh37") {
        assembly = "hg19"
        chrs = sprintf("chr%s", chrs)
        strip_chr_prefix = TRUE
    } else
        strip_chr_prefix = FALSE

    masked = .genome(assembly, masked=TRUE)

    if (length(chrs) == 0)
        chrs = names(masked)

    masked2granges = function(chr_name) {
        chr = masked[[chr_name]]
        IRanges::active(Biostrings::masks(chr)) = FALSE
        IRanges::active(Biostrings::masks(chr))[mask] = TRUE
        GenomicRanges::GRanges(seqnames = chr_name,
                               ranges = IRanges::ranges(as(chr, "XStringViews")))
    }
    ranges = lapply(chrs, masked2granges)
    ranges = suppressWarnings(do.call(c, ranges))
    GenomeInfoDb::seqinfo(ranges) = GenomeInfoDb::seqinfo(masked)

    if (strip_chr_prefix) {
        newlvls = GenomeInfoDb::mapSeqlevels(GenomeInfoDb::seqlevels(ranges), "NCBI")
        GenomeInfoDb::renameSeqlevels(ranges, newlvls)
    }

    ranges
}

genome_mask.MaskedBSgenome = function(assembly, chrs=NULL,
                         mask=c("AGAPS", "AMB", "TRF")) {
    if (length(chrs) == 0)
        chrs = names(assembly)

    masked2granges = function(chr_name) {
        chr = assembly[[chr_name]]
        IRanges::active(Biostrings::masks(chr)) = FALSE
        IRanges::active(Biostrings::masks(chr))[mask] = TRUE
        GenomicRanges::GRanges(seqnames = chr_name,
                               ranges = IRanges::ranges(as(chr, "XStringViews")))
    }
    ranges = lapply(chrs, masked2granges)
    ranges = suppressWarnings(do.call(c, ranges))
    GenomeInfoDb::seqinfo(ranges) = GenomeInfoDb::seqinfo(assembly)

    ranges
}
