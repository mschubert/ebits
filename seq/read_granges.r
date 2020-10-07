.read_bed = import('./read_bed')$read_bed
.read_bam = import('./read_bam')$read_bam

#' Read a BAM or BED file to read_granges object
#'
#' @param fname  A character vector of file name(s) of BAM or BED files
#' @param ...    Arguments passed to AneuFinder
read_granges = function(fname, ...) {
    UseMethod("read_granges")
}

read_granges.list = function(fnames, ...) {
    lapply(fnames, read_granges, ...)
}

read_granges.character = function(fname, ...) {
    args = list(...)
    if (grepl("\\.bam$", fname)) {
        .read_bam(fname, ...)
    } else if (grepl("\\.bed(\\.gz)?$", fname)) {
        .read_bed(fname, ...)
    } else
        stop("Do not know how to read file type: ", fname)
}

read_granges.default = function(fname, ...) {
    stop("Do not know how to handle argument of class: ", sQuote(class(fname)))
}
