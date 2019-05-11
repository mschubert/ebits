.genome = import('./genome')$genome

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
    if (assembly == "GRCh37")
        strip_chr_prefix = TRUE
    else
        strip_chr_prefix = FALSE

    chr_lengths = GenomeInfoDb::seqlengths(.genome(assembly))

    if (strip_chr_prefix)
        names(chr_lengths) = sub("^chr", "", names(chr_lengths))

    if (!is.null(chrs))
        chr_lengths = chr_lengths[names(chr_lengths) %in% chrs]

    chr_lengths
}

chr_lengths.Seqinfo = function(assembly, chrs=NULL) {
    chr_lengths = GenomeInfoDb::seqlengths(assembly)

    if (!is.null(chrs))
        chr_lengths = chr_lengths[names(chr_lengths) %in% chrs]

    chr_lengths
}

chr_lengths.BSgenome = function(assembly, chrs=NULL) {
    chr_lengths = GenomeInfoDb::seqlengths(assembly)

    if (!is.null(chrs))
        chr_lengths = chr_lengths[names(chr_lengths) %in% chrs]

    chr_lengths
}
