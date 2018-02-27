.sys = import('../../sys')

`.seqinfo<-` = function(x, value) {
    idx = unique(as.character(GenomeInfoDb::seqnames(x)))
    x@seqinfo = value[idx]
    if (inherits(x, "GRanges"))
        x@seqnames = droplevels(x@seqnames)
    x
}

#' Combine multivariate models
#'
#' @param models  Univariate Chromstar models ('info' field: exp_table row)
#' @param mode    A character string that describes how to build the model:
#'    'separate': each condition and mark separately
#'    'combinatorial': combined model of different marks in one condition
#'    'differential': combined model for one mark in different condition/sample
#'    'full': combined conditions and marks
#' @param marks    Which marks to include from table (default: all)
#' @param samples  Which conditions to include from table (default: all)
#' @return  Combined Chromstar result
combine = function(models, mode, marks=NULL, samples=NULL) {
    # make sure all models have the same genome
    sinfo = unique(lapply(models, function(m) GenomeInfoDb::seqinfo(m$peaks)))
    if (length(sinfo) != 1)
        stop("models must all have same seqinfo: ", paste(sinfo, collapse=", "))
    sinfo = sinfo[[1]]

    # use only models that have requested marks and conditions
    if (!is.null(marks))
        models = models[sapply(models, function(m) m$info$mark %in% marks)]
    if (!is.null(samples))
        models = models[sapply(models, function(m) m$info$condition %in% samples)]

    # create combined experiment table
    exp_table = dplyr::bind_rows(lapply(models, function(m) m$info))
    names(models) = exp_table$ID
    brew_mode = mode

    # create experiment table for conditions
    switch(mode,
        "separate" = {
            reps = paste0(exp_table$mark, '-', exp_table$condition)
            exps = split(exp_table, reps)
            brew_mode = "combinatorial"
            mode = "replicate"
        },
        "full" = {
            exps = list(exp_table)
        },
        "combinatorial" = {
            exps = split(exp_table, exp_table$condition)
        },
        "differential" = {
            exps = split(exp_table, exp_table$mark)
        }
    )

    # combine models
    multi = lapply(exps, function(exp) {
        re = chromstaR::callPeaksMultivariate(
            hmms = models[exp$ID],
            use.states = chromstaR::stateBrewer(select(exp, -ID), mode=brew_mode))
        re
    })
    cmb = chromstaR::combineMultivariates(multi, mode=mode)

    # annotate GRanges objects with seqinfo information
    .seqinfo(cmb$segments) = sinfo
    for (i in seq_along(cmb$peaks))
        .seqinfo(cmb$peaks[[i]]) = sinfo

    cmb
}

.sys$run({
    io = import('../../io')
    import('.', attach=TRUE)

    args = .sys$cmd$parse(
        opt('s', 'samples', 'file containing sample table', 'SBT003.yaml'),
        opt('u', 'unifile', 'file to load univariate models from', '/dev/null'),
        opt('o', 'outfile', 'file to save model to', '/dev/null'),
        opt('b', 'beddir', 'dir to save tracks to', '/dev/null'),
        opt('p', 'plotfile', 'file to save plots to', '/dev/null'))

    print(args)

    config = io$read_yaml(args$samples)
    univ = io$load(args$unifile)

    do_cmp = function(cmp) do.call(combine, c(list(models=univ), cmp))
    models = lapply(config$comparisons, do_cmp) #TODO: filter models here if they fail later?

    if (!is.null(args$outfile))
        save(models, file=args$outfile)

    if (!is.null(args$beddir)) {
        dir.create(args$beddir)
        for (i in seq_along(models)) {
            message(i,"/",length(models))
            try(chromstaR:::exportCombinedMultivariateCombination(models[[i]],
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
