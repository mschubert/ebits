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
    chr_lengths(.genome(assembly), chrs=chrs)
}

# .Seqinfo, .BSgenome
chr_lengths.default = function(assembly, chrs=NULL) {
    if (is.null(chrs))
        chrs = GenomeInfoDb::standardChromosomes(assembly)

    chr_lengths = GenomeInfoDb::seqlengths(assembly)
    chr_lengths[names(chr_lengths) %in% chrs]
}
