import_package('dplyr', attach=TRUE)
.lengths = import('./chr_lengths')

#' Provide AneuFinder-like aneuploidy score
#'
#' @param ranges  GRanges object or data.frame with copy numbers
#' @param per_chromosome  Calculate aneuploidy score per chromosome or genome
#' @param assembly  Character string of ID or GenomeInfoDb assembly object
#' @return        Aneuploidy scores for each sample in `ranges`
aneuploidy = function(ranges, assembly, per_chromosome=FALSE,
                      chr_exclude = c("X", "Y", "MT"), ...) {
    UseMethod("aneuploidy")
}

#' @rdname aneuploidy
aneuploidy.GRanges = function(ranges, per_chromosome=FALSE, assembly="GRCh38",
                              chr_exclude = c("X", "Y", "MT")) {
    aneuploidy(as.data.frame(ranges))
}

#' @rdname aneuploidy
aneuploidy.data.frame = function(ranges, per_chromosome=FALSE, assembly="GRCh38",
         chr_exclude = c("X", "Y", "MT"), width="width",
         chromosome="seqnames", copies="ploidy", sample="Sample") {
    chrs = .lengths$chr_lengths(assembly)
    chrs = data.frame(seqnames=names(chrs), size=unname(as.numeric(chrs)))

    cna = ranges %>%
        filter(! seqnames %in% chr_exclude) %>%
        mutate(width = as.numeric(width),
               euploid_dev = abs(2 - ploidy)) %>%
        group_by(Sample, seqnames) %>%
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
            group_by(Sample) %>%
            summarize(aneuploidy = stats::weighted.mean(aneuploidy, covered),
                      coverage = sum(covered) / sum(size))
    }
}
