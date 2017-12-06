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
    AneuFinder::bam2GRanges(fname, assembly, ...)
}

read_bed.default = function(fname, assembly, ...) {
    stop("Do not know how to handle argument of class: ", sQuote(class(fname)))
}
