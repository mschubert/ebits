run = import('./run')$run
bin_reads = import('./bin_reads')$bin_reads
blacklist_from_ref = import('./blacklist_from_ref')$blacklist_from_ref
consensus_ploidy = import('./consensus_ploidy')$consensus_ploidy
plot = import('./plot')$plot
.sys = import('../../sys')

.sys$run({
    import_package('dplyr', attach=TRUE)
    io = import('../../io')

    args = .sys$cmd$parse(
        opt('d', 'directory', 'directory prefix to look for files', '.'),
        opt('o', 'outfile', 'file to save model to', 'model.RData'),
        opt('p', 'plotfile', 'file to save plots to', 'model.pdf'),
        opt('a', 'assembly', 'genome assembly identifier', ''),
        opt('b', 'bin_width_ref', 'euploid reference for bin width', ''),
        opt('r', 'min_reads', 'minimum numer of reads for sample', '5e4'),
        arg('infiles', 'BAM/BED files, directory or sample file', arity='*'))

    if (length(args$infiles) == 1 && grepl("\\.yaml$", args$infiles)) {
        config = io$read_yaml(args$infiles)
        args$infiles = file.path(config$directory, config$files)
        args$assembly = config$assembly
        args$bin_width_ref = config$bin_width_ref
        args$blacklist = config$blacklist
    }

    print(args)

    message("Running aneufinder ...")
    all_models = run(args$infiles,
                     assembly = args$assembly,
                     bin_width_ref = args$bin_width_ref,
                     blacklist = args$blacklist)
    names(all_models) = sapply(all_models, function(x) x$ID)

    nreads = sapply(all_models, function(m) sum(m$bincounts[[1]]$counts)) %>%
        setNames(names(all_models))
    excl_reads = nreads[nreads < as.numeric(args$min_reads)]
    excl_reads = setNames(paste(excl_reads, "reads"), names(excl_reads))
    exclude = c(unlist(config$exclude), excl_reads)

    models = all_models[!names(all_models) %in% names(exclude)]
    if (length(exclude) > 0) {
        warning("Dropping models: ", paste(names(exclude), collapse=", "))
        for (i in seq_along(all_models)) {
            model_name = names(all_models)[i]
            excl_idx = which(names(exclude) == model_name)
            if (length(excl_idx) > 0) {
                reason = paste(exclude[excl_idx], collapse=", ")
                all_models[[i]]$ID = sprintf("%s [%s]", model_name, reason)
            }
        }
    }
    save(models, file=args$outfile)

    if (!is.null(args$plotfile)) {
        message("Plotting sample heatmap ...")

        pdf(args$plotfile, width=20, height=10)

        # plot models we keep
        print(AneuFinder::heatmapGenomewide(models, cluster=TRUE))

        # plot all models to see why we exclude
        if (length(exclude) > 0)
            print(AneuFinder::heatmapGenomewide(all_models, cluster=TRUE))

        # plot individual model read bin counts, distributions, etc.
        for (i in seq_along(all_models)) {
            message("Read density plot for: ", names(all_models)[i])
            if (class(all_models[[i]]) == "aneuHMM")
                print(plot(all_models[[i]]))
        }

        dev.off()
    }
})
