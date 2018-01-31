seq = import('seq')

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
