.genome = import('./genome')$genome

#' Get chromosome lengths from assembly ID
#'
#' @param assembly     An assembly identifier, e.g. 'hg19' or 'GRCh38' or
#'                     assembly object from 'seq$genome(...)'
#' @param chromosomes  Only return the following chromosomes (default: all)
#' @return             A named vector of chromosome lengths
assembly_info = function(assembly, chromosomes=NULL) {
    UseMethod("assembly_info")
}

assembly_info.character = function(assembly, chromosomes=NULL) {
    if (assembly == "GRCh37")
        strip_chr_prefix = TRUE
    else
        strip_chr_prefix = FALSE

    chr_lengths = GenomeInfoDb::seqlengths(.genome(assembly))

    if (strip_chr_prefix)
        names(chr_lengths) = sub("^chr", "", names(chr_lengths))

    if (!is.null(chromosomes))
        chr_lengths = chr_lengths[names(chr_lengths) %in% chromosomes]

    chr_lengths
}
