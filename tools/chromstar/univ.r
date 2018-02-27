seq = import('../../seq')
.sys = import('../../sys')

`.seqinfo<-` = function(x, value) {
    idx = unique(as.character(GenomeInfoDb::seqnames(x)))
    x@seqinfo = value[idx]
    if (inherits(x, "GRanges"))
        x@seqnames = droplevels(x@seqnames)
    x
}

#' Run chromstar in univariate mode
#'
#' @param binned_reads  A list of GRanges objects represnting binned reads
#' @return              A list of univariate Chromstar model objects
univ = function(binned_reads, exp_table) {
    exp_table$ID = sprintf("%s-%s-rep%s", exp_table$mark,
                           exp_table$condition, exp_table$replicate)
    univ = split(exp_table, seq_len(nrow(exp_table)))
    lapply(univ, function(exp) {
        m = chromstaR::callPeaksUnivariate(
            binned.data = binned_reads[[exp$file]],
            input.data = binned_reads[[exp$controlFiles]],
            keep.posteriors = FALSE)
        # lengths are copied, genome is not
        .seqinfo(m$bins) = seq$info(binned_reads[[1]])
        .seqinfo(m$peaks) = seq$info(binned_reads[[1]])
        m$info = exp
        m
    })
}

.sys$run({
    io = import('../../io')
    import('.', attach=TRUE)

    args = .sys$cmd$parse(
        opt('d', 'directory', 'directory containing source files', '.'),
        opt('s', 'samples', 'file containing sample table', 'SBT003.yaml'),
        opt('o', 'outfile', 'file to save univariate model to', '/dev/null'),
        opt('r', 'readfile', 'file to save reads to', '/dev/null'),
        opt('b', 'beddir', 'dir to save tracks to', '/dev/null'),
        opt('p', 'plotfile', 'file to save plots to', '/dev/null'))

    print(args)

    config = io$read_yaml(args$samples)
    exp_table = parse_spec(config)
    print(exp_table)

    reads = read_files(exp_table, config$assembly)
    if (args$readfile != "/dev/null")
        save(reads, file=args$readfile)
    binned = bin_reads(reads)
    rm(reads)

    univ = univ(binned, exp_table)
    if (args$outfile != "/dev/null")
        save(univ, file=args$outfile)

    if (!is.null(args$beddir)) {
        dir.create(args$beddir)
        for (i in seq_along(univ)) {
            message(i,"/",length(univ))
            try(chromstaR:::exportUnivariateCounts(univ[[i]],
                filename=file.path(args$beddir, names(univ)[i]),
                separate.files=FALSE))
        }
    }

    if (!is.null(args$plotfile)) {
        pdf(args$plotfile)
        message("read count cor")
        print(plot$cor(univ))
        for (i in seq_along(univ)) {
            message(sprintf("univariate fits (%i/%i)", i, length(univ)))
            print(chromstaR::plotHistogram(univ[[i]]))
        }
        dev.off()
    }
})
