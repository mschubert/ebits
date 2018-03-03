import_package('dplyr', attach=TRUE)
.lengths = import('./chr_lengths')

#' Calculate aneuploidy score from diploid deviation along chromosomes
#'
#' This will calculated the average deviation from the diploid state of copy
#' number segments inferred by e.g. SNP6 arrays or DNA-seq.
#'
#' @param ranges  GRanges object or data.frame with copy numbers
#' @param per_chromosome  Calculate aneuploidy score per chromosome or genome
#' @param assembly  Character string of ID or GenomeInfoDb assembly object
#' @param chromosomes  Character vector of chromosomes to use
#' @param chr_exclude  Character vector of chromosomes to exclude (only used
#'      if chromosomes=NULL)
#' @return        Aneuploidy scores for each sample in `ranges`
aneuploidy = function(ranges, assembly, per_chromosome=FALSE,
            chromosomes=NULL, chr_exclude = c("X", "Y", "MT"), ...) {
    UseMethod("aneuploidy")
}

#' @rdname aneuploidy
aneuploidy.GRanges = function(ranges, per_chromosome=FALSE, assembly="GRCh38",
            chromosomes=NULL, chr_exclude = c("X", "Y", "MT"), ...) {
    aneuploidy(as.data.frame(ranges))
}

#' @rdname aneuploidy
aneuploidy.data.frame = function(ranges, per_chromosome=FALSE, assembly="GRCh38",
         chromosomes=NULL, chr_exclude = c("X", "Y", "MT"), width="width",
         seqnames="seqnames", ploidy="ploidy", sample="Sample") {
    chrs = .lengths$chr_lengths(assembly)
    chrs = data.frame(seqnames=names(chrs), size=unname(as.numeric(chrs)))

    syms = rlang::syms(c(seqnames, ploidy, sample, width)) %>%
        setNames(c("seqnames", "ploidy", "sample", "width"))

    if (is.null(chromosomes))
        chromosomes = setdiff(ranges[[seqnames]], chr_exclude)

    cna = ranges %>%
        mutate(seqnames = as.character(!! syms[["seqnames"]]),
               width = as.numeric(!! syms[["width"]]),
               euploid_dev = abs(2 - !! syms[["ploidy"]])) %>%
        filter(seqnames %in% chromosomes) %>%
        group_by(!! syms[["sample"]], seqnames) %>%
        summarize(covered = sum(width),
                  aneuploidy = stats::weighted.mean(euploid_dev, width)) %>%
        left_join(chrs, by="seqnames") %>%
        ungroup()

    if (per_chromosome) {
        re = cna %>%
            mutate(coverage = covered / size) %>%
            select(-covered, -size)
    } else {
        re = cna %>%
            group_by(!! syms[["sample"]]) %>%
            summarize(aneuploidy = stats::weighted.mean(aneuploidy, covered),
                      coverage = sum(covered) / sum(size))
    }
}
