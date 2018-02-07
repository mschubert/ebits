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
    models = lapply(config$comparisons, do_cmp)

    if (!is.null(args$outfile))
        save(models, file=args$outfile)

    if (!is.null(args$beddir)) {
        dir.create(args$beddir)
        for (i in seq_along(models)) {
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

        print(plot$cor(univ))
        for (u in univ)
            print(chromstaR::plotHistogram(u))

        for (i in seq_along(models)) {
            m = models[[i]]
            mn = names(models)[i]

#            for (pp in c(NA, 1e-4, 1e-10)) {
#                if (is.na(pp))
                    mod_tit = sprintf("%s (mode=%s, max posterior probability)",
                                      mn, config$comparisons[[mn]]$mode)
#                else {
#                    mod_tit = sprintf("%s (mode=%s, pp=%.0g)",
#                                      mn, config$comparisons[[mn]]$mode, pp)
#                    m = filter_peaks(m, pp)
#                }
                mod_desc = sprintf("%s; %s",
                                   paste(unique(m$info$mark), collapse=","),
                                   paste(unique(m$info$condition), collapse=","))
                print(plot$frequencies(m) +
                      labs(title=mod_tit, subtitle=mod_desc))

        #        print(plot$enrichment(model, ref="TSS"))
                ps = plot$enrichment(m, dset=dset, ref="gene")
                for (j in seq_along(ps)) {
                    tit = paste(names(ps)[j], mod_tit)
                    print(ps[[j]] + labs(title=tit, subtitle=mod_desc))
                }
#            }
        }

        dev.off()
    }
})
