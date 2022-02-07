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

read_bam.character = function(fname, paired_reads=TRUE, use_names=TRUE, remove_duplicates=FALSE, assembly=NULL) {
    bai = paste0(fname, ".bai")
    # There is no real reason we should have to sort a bam in order to read
    # the reads into a GRanges object. However, the GenomicAlignments::readGAlignments
    # function needs it, so let's sort/index until we find a better option.
    if (!file.exists(bai)) {
        message("[read_bam] creating index file")
        idx = try(Rsamtools::indexBam(fname))
        if (class(idx) == "try-error") {
            tmp = file.path(tempdir(), basename(fname))
            message("[read_bam] indexing failed, creating temporary sorted bam")
            Rsamtools::sortBam(fname, tools::file_path_sans_ext(tmp))
            on.exit(unlink(tmp))
        }
        Rsamtools::indexBam(tmp)
        fname = tmp
        bai = paste0(fname, ".bai")
    }

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

    remove_duplicates = Rsamtools::scanBamFlag()
    if (remove_duplicates)
        flag = Rsamtools::scanBamFlag(isDuplicate = FALSE)
    param = Rsamtools::ScanBamParam(flag=flag) #, which=range(gr), what=what, mapqFilter=min.mapq)

    if (paired_reads) {
        reads = GenomicAlignments::readGAlignmentPairs(fname, use.names=use_names, index=bai, param=param)
    } else {
        reads = GenomicAlignments::readGAlignments(fname, use.names=use_names, index=bai, param=param)
    }
    if (length(reads) == 0)
        stop("No reads in ", sQuote(fname), " (looking for PE in SE file?)")

    GenomicAlignments::granges(reads, use.mcols=TRUE)
}

read_bam.default = function(fname, ...) {
    stop("Do not know how to handle argument of class: ", sQuote(class(fname)))
}
