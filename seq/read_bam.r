.chr_lengths = import('./chr_lengths')$chr_lengths

#' Read a BAM or BED file to read_bam object
#'
#' @param fname  A character vector of file name(s) of BAM files
#' @param ...    Arguments passed to AneuFinder
read_bam = function(fname, ...) {
    UseMethod("read_bam")
}

read_bam.list = function(fnames, ...) {
    lapply(fnames, read_bam, ...)
}

read_bam.character = function(fname, ..., assembly=NULL) {
    bai = paste0(fname, ".bai")
    if (file.exists(bai))
        bamindex = bai
    else
        bamindex = fname

    if (!is.null(assembly)) {
        ff = .chr_lengths(Rsamtools::BamFile(fname))
        fa = .chr_lengths(assembly)

        cmp = merge(data.frame(chr=names(ff), file=unname(ff)),
                    data.frame(chr=names(fa), assembly=unname(fa)),
                    by="chr")

        if (!all(cmp$file == cmp$assembly, na.rm=TRUE)) {
            print(cmp)
            stop("Assembly length mismatch")
        }
    }

    AneuFinder::bam2GRanges(fname, bamindex, ...)
}

read_bam.default = function(fname, ...) {
    stop("Do not know how to handle argument of class: ", sQuote(class(fname)))
}
