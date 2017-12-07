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
        if ("assembly" %in% names(args)) {
            slen = GenomeInfoDb::seqlengths(Rsamtools::BamFile(fname))
            assembly = args$assembly

            if (is.data.frame(assembly))
                assembly = setNames(assembly$length, assembly$chromosome)

            if (!is.null(args$chromosomes))
                assembly = assembly[names(assembly) %in% args$chromosomes]

            cmp = merge(data.frame(chr=names(slen), file=slen),
                        data.frame(chr=names(assembly), assembly=assembly),
                        by="chr")

            if (!all(cmp$file == cmp$assembly)) {
                print(cmp)
                stop("Assembly length mismatch")
            }
        }

        used_args = args[setdiff(names(args), "assembly")]
        do.call(AneuFinder::bam2GRanges, c(list(bamfile=fname), used_args))

    } else if (grepl("\\.bed(\\.gz)?$", fname)) {
        used_args = args[setdiff(names(args), "bamindex")]
        do.call(AneuFinder::bed2GRanges, c(list(bedfile=fname), used_args))
    } else
        stop("Do not know how to read file type: ", fname)
}

read_granges.default = function(fname, ...) {
    stop("Do not know how to handle argument of class: ", sQuote(class(fname)))
}
