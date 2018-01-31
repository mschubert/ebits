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
