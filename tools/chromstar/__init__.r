parse_spec = import('./parse_spec')$parse_spec
read_files = import('./read_files')$read_files
bin_reads = import('./bin_reads')$bin_reads
split_granges = import('./split_granges')$split_granges
univ = import('./univ')$univ
combine = import('./combine')$combine
filter_peaks = import('./filter_peaks')$filter_peaks
plot = import('./plot')

.sys = import('../../sys')

.sys$run({
    io = import('../../io')

    args = .sys$cmd$parse(
        opt('d', 'directory', 'directory containing source files', '.'),
        opt('s', 'samples', 'file containing sample table', 'SBT003.yaml'),
        opt('o', 'outfile', 'file to save model to', '/dev/null'),
        opt('u', 'unifile', 'file to save univariate model to', '/dev/null'),
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
    if (args$unifile != "/dev/null")
        save(univ, file=args$unifile)

    do_cmp = function(cmp) do.call(combine, c(list(models=univ), cmp))
    models = lapply(config$comparisons, do_cmp) #TODO: filter models here if they fail later?

    if (!is.null(args$outfile))
        save(models, file=args$outfile)

    if (!is.null(args$beddir)) {
        dir.create(args$beddir)
        for (i in seq_along(models)) {
            message(i,"/",length(models))
            try(chromstaR:::exportCombinedMultivariatePeaks(models[[i]],
                filename=file.path(args$beddir, names(models)[i]),
                separate.files=FALSE))
        }
    }

    if (!is.null(args$plotfile)) {
        if (grepl("GRCh|hg", config$assembly))
            dset = "hsapiens_gene_ensembl"
        else if (grepl("GRCm|mm", config$assembly))
            dset = "mmusculus_gene_ensembl"
        else
            stop("do not know which gene annotation to use, can not plot")

        pdf(args$plotfile)
        message("read count cor")
        print(plot$cor(univ))
        for (i in seq_along(univ)) {
            message(sprintf("univariate fits (%i/%i)", i, length(univ)))
            print(chromstaR::plotHistogram(univ[[i]]))
        }

        for (i in seq_along(models)) {
            m = models[[i]]
            mn = names(models)[i]
            message(sprintf("freqs: %s (%i/%i)", mn, i, length(models)))

            mod_tit = sprintf("%s (mode=%s, max posterior probability)",
                              mn, config$comparisons[[mn]]$mode)
            mod_desc = sprintf("%s; %s",
                               paste(unique(m$info$mark), collapse=","),
                               paste(unique(m$info$condition), collapse=","))
            print(plot$frequencies(m) +
                  ggplot2::labs(title=mod_tit, subtitle=mod_desc))

            message(sprintf("enrichment: %s (%i/%i)", mn, i, length(models)))
            ps = plot$enrichment(m, dset=dset, ref="gene")
            for (j in seq_along(ps)) {
                tit = paste(names(ps)[j], mod_tit)
                print(ps[[j]] + ggplot2::labs(title=tit, subtitle=mod_desc))
            }
        }
        dev.off()
    }
})
