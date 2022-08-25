.genome = import('./genome')$genome
`%>%` = magrittr::`%>%`

#' Get chromosome lengths from assembly ID
#'
#' @param assembly  An assembly identifier, e.g. 'hg19' or 'GRCh38' or
#'                  assembly object from 'seq$genome(...)'
#' @param chrs      Only return the following chromosomes (default: all)
#' @return          A named vector of chromosome lengths
chr_lengths = function(assembly, chrs=NULL) {
    UseMethod("chr_lengths")
}

chr_lengths.character = function(assembly, chrs=NULL) {
    chr_lengths(.genome(assembly), chrs=chrs)
}

# .Seqinfo, .BSgenome
chr_lengths.default = function(assembly, chrs=NULL) {
    if (is.null(chrs))
        chrs = GenomeInfoDb::standardChromosomes(assembly)

    chr_lengths = GenomeInfoDb::seqlengths(assembly)
    chr_lengths[names(chr_lengths) %in% chrs]
}

#' Generate stepwise data points along chromosomes
#'
#' @param step  Step size in bases (default: 1e7)
#' @param assembly  Which assembly to use (default: GRCh38)
#' @param chrs  Chromosomes to include (default: 1:22,X)
#' @param min_n  Minimum number of data points per chromosome
#' @return  data.frame with fields: chr, length, steps [list of numeric]
chr_step = function(step=1e7, assembly="GRCh38", chrs=NULL, min_n=7) {
    chr_lengths(assembly=assembly, chrs=chrs) %>%
        stack() %>%
        dplyr::select(chr=ind, length=values) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(steps = list(seq(1, length, length.out=max(min_n, round(length/step))))) %>%
        dplyr::select(-length)
}

#' Convert chromosome positions to total positions
#'
#' @param chr  Vector of chromosomes
#' @param pos  Vector of chromosome positions
#' @return     Numeric vector with absolute chromosome positions
chr2total_pos = function(chr, pos) {
    old = data.frame(chr=chr, pos=pos)
    add = chr_lengths() %>%
        dplyr::transmute(chr = chr,
                         add_len = c(0, cumsum(as.numeric(length))[-nrow(.)]))

    dplyr::left_join(old, add, by="chr") %>%
        dplyr::mutate(pos = pos + add_len) %>%
        dplyr::pull(pos)
}
