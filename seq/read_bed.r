.chr_lengths = import('./chr_lengths')$chr_lengths

#' Read a BAM or BED file to read_bed object
#'
#' @param fname     A character vector of file name(s) of BED files
#' @param assembly  Data frame with fields 'chromosome' and 'length'
#' @param ...       Arguments passed to AneuFinder
read_bed = function(fname, assembly, ...) {
    UseMethod("read_bed")
}

read_bed.list = function(fnames, assembly, ...) {
    lapply(fnames, read_bed, assembly=assembly, ...)
}

read_bed.character = function(fname, assembly, ...) {
    if (!is.data.frame(assembly) && !is.vector(assembly))
        assembly = .chr_lengths(assembly)
    if (is.vector(assembly) && !is.null(names(assembly)))
        assembly = data.frame(chromosome=names(assembly), length=unname(assembly))

    # AneuFinder needs 'assembly' to be a data.frame with 'chromosomes', 'length'
    AneuFinder::bed2GRanges(fname, assembly=assembly, ...)
}

read_bed.default = function(fname, assembly, ...) {
    stop("Do not know how to handle argument of class: ", sQuote(class(fname)))
}
