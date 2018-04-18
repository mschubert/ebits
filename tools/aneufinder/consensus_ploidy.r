import_package('dplyr', attach=TRUE)

#' Calculate the consensus ploidy state per chromosome across single cell models
#'
#' @param mods  List of AneuFinder models or GRanges objects
#' @return      Data.frame with fields: seqnames (chromosome name),
#'              length (chromosome length, coverage (% of chr covered by bins),
#'              and ploidy (average ploidy along chromosome)
consensus_ploidy = function(mods) {
    if (class(mods) != 'list')
        mods = list(mods)
    if (class(mods[[1]]) == "aneuHMM")
        mods = lapply(mods, function(m) m$segments)

    suppressWarnings(mods %>%
        lapply(as_tibble) %>%
        dplyr::bind_rows() %>%
        group_by(seqnames) %>%
        summarize(ploidy = weighted.mean(copy.number, width),
                  bases_covered = sum(as.numeric(width)) / length(mods)) %>%
        transmute(seqnames = as.character(seqnames),
                  length = unname(GenomeInfoDb::seqlengths(mods[[1]])[seqnames]),
                  coverage = bases_covered / length,
                  ploidy = ploidy))
}
